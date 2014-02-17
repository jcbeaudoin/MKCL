/* -*- mode: c -*- */
/*
    unixsys.s  -- Unix shell interface.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2012, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <mkcl/internal.h>
#include <mkcl/mkcl-inl.h>

#if defined(MKCL_WINDOWS)
# include <windows.h>
#endif

#ifdef HAVE_UNISTD_H
# include <sys/types.h>
# include <unistd.h>
# ifdef __linux
#  include <sys/syscall.h>
# endif
#endif

#ifdef HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif

#ifdef __unix
static pthread_mutex_t children_list_lock;

static inline void CHILDREN_LIST_LOCK(MKCL)
{
  if (pthread_mutex_lock(&children_list_lock))
    mkcl_lose(env, "Failed in CHILDREN_LIST_LOCK()");
}

static inline void CHILDREN_LIST_UNLOCK(MKCL)
{
  if (pthread_mutex_unlock(&children_list_lock))
    mkcl_lose(env, "Failed in CHILDREN_LIST_UNLOCK()");
}
#endif

void mkcl_safe_close(MKCL, int fd, mkcl_object stream)
{
#if __linux
  int status = 0;
  mkcl_interrupt_status old_intr;

  mkcl_get_interrupt_status(env, &old_intr);
  mkcl_disable_interrupts(env);
  while ((status = close(fd)) < 0)
    if (errno != EINTR) break;
  mkcl_set_interrupt_status(env, &old_intr);
  if (status)
    mkcl_FElibc_stream_error(env, stream, "mkcl_safe_close failed.", 0);
#elif defined(MKCL_WINDOWS)
  if (_close(fd))
    mkcl_FEwin32_stream_error(env, stream, "mkcl_safe_close failed.", 0);
#else
#error "Don't know how to implement mkcl_safe_close()!"
#endif
}

#if 0
void mkcl_safe_fclose(MKCL, FILE * s, mkcl_object stream)
#else
void mkcl_safe_fclose(MKCL, void * vs, mkcl_object stream)
#endif
{
  FILE * s = vs;
#if __linux
  int status = 0;
  mkcl_interrupt_status old_intr;

  mkcl_get_interrupt_status(env, &old_intr);
  mkcl_disable_interrupts(env);
  while ((status = fclose(s)) == EOF)
    if ((errno != EINTR) && (errno != EAGAIN) && (errno != EWOULDBLOCK)) break;
  mkcl_set_interrupt_status(env, &old_intr);

  if (status)
    mkcl_FElibc_stream_error(env, stream, "mkcl_safe_fclose() failed.", 0);
#elif defined(MKCL_WINDOWS)
  if (fclose(s) == EOF)
    mkcl_FEwin32_stream_error(env, stream, "mkcl_safe_fclose() failed.", 0);
#else
#error "Don't know how to implement mkcl_safe_fclose()!"
#endif
}

#ifdef MKCL_WINDOWS

static mkcl_object read_command_output(MKCL, HANDLE child_stdout_read)
{
  mkcl_object output = mk_cl_Cnil;
  mkcl_object out_tail = mk_cl_Cnil;

  while (TRUE)
    {
      CHAR buffer[16 * 1024];
      DWORD read_count = 0;
      mkcl_object paragraph;

      if (!ReadFile(child_stdout_read, buffer,
		    sizeof(buffer) - sizeof(buffer[0]),
 		    &read_count, NULL)
	  || (read_count == 0))
	{
	  if (GetLastError() == ERROR_BROKEN_PIPE)
	    break; /* EOF on pipe - normal exit. */
	  else
	    mkcl_FEwin32_error(env, "read_command_output failed on ReadFile", 0);
	}

      buffer[read_count/sizeof(buffer[0])] = 0; /* mark end of string. */
      /* We're expecting the subprocess to emit its output in UTF-8. */
      mkcl_UTF_8_object_sized(utf_8_obj, buffer, read_count/sizeof(buffer[0]));
      paragraph = mkcl_utf_8_to_string(env, (mkcl_object) &utf_8_obj);

      /* accumulate output */
      if (mkcl_Null(out_tail))
	output = out_tail = MKCL_CONS(env, paragraph, mk_cl_Cnil);
      else
	{
	  mkcl_object this = MKCL_CONS(env, paragraph, mk_cl_Cnil);

	  MKCL_RPLACD(out_tail, this);
	  out_tail = this;
	}
    }
  return output;
}

@(defun mkcl::run-command (command directory &key real_name)
@
  mkcl_dynamic_extent_OSstring(env, os_command, command);
  mkcl_dynamic_extent_OSstring(env, os_new_directory, (mkcl_Null(directory) ? mkcl_core.empty_string : directory));
  wchar_t * os_raw_new_directory = NULL;
  mkcl_object output;
  STARTUPINFOW StartUp;
  PROCESS_INFORMATION ProcInfo;
  DWORD ExitCode;
  BOOL success;

  HANDLE child_stdout_read, child_stdout_read_tmp, child_stdout_write;
  HANDLE child_stdin, child_stderr;
  SECURITY_ATTRIBUTES sec_attr;
  HANDLE current = GetCurrentProcess(); /* pseudo handle */

  sec_attr.nLength = sizeof(SECURITY_ATTRIBUTES);
  sec_attr.lpSecurityDescriptor = NULL;
  sec_attr.bInheritHandle = TRUE;

  /* create stdout */
  if (!CreatePipe(&child_stdout_read_tmp, &child_stdout_write, &sec_attr, 0))
    mkcl_FEwin32_error(env, "mkcl::run-command failed on CreatePipe", 0);

  /* create stderr, handle has bInheritHandle to TRUE. */
  if (!DuplicateHandle(current, child_stdout_write, current, &child_stderr,
		       0, TRUE, DUPLICATE_SAME_ACCESS))
    mkcl_FEwin32_error(env, "mkcl::run-command failed on DuplicateHandle", 0);

  /* create stdin */
  child_stdin = CreateFileW(L"NUL", GENERIC_READ,
			    FILE_SHARE_READ | FILE_SHARE_WRITE,
			    &sec_attr, OPEN_EXISTING, 0, NULL);
  if (child_stdin == INVALID_HANDLE_VALUE)
    mkcl_FEwin32_error(env, "mkcl::run-command failed to open NUL device", 0);

  /* create stderr, handle has bInheritHandle to FALSE. */
  if (!DuplicateHandle(current, child_stdout_read_tmp, current, &child_stdout_read,
		       0, FALSE, DUPLICATE_SAME_ACCESS))
    mkcl_FEwin32_error(env, "mkcl::run-command failed on DuplicateHandle", 0);
  if (!CloseHandle(child_stdout_read_tmp))
    mkcl_FEwin32_error(env, "mkcl::run-command failed on CloseHandle", 0);


  ZeroMemory(&StartUp, sizeof(STARTUPINFO));
  StartUp.cb = sizeof(STARTUPINFO);
  StartUp.dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
  StartUp.wShowWindow = SW_HIDE;
  StartUp.hStdOutput = child_stdout_write;
  StartUp.hStdInput = child_stdin;
  StartUp.hStdError = child_stderr;

  /* Microsoft's documentation says that the maximum size for
     the "command" string is 32768 characters, all included.
     And the first component of it must be less than MAX_PATH.
     FIXME!
  */
  if (!mkcl_Null(directory))
    {
      os_raw_new_directory = mkcl_OSstring_self(os_new_directory);
    }

  if (!mkcl_Null(real_name))
    {
      mkcl_dynamic_extent_OSstring(env, os_command_real_name, real_name);

      success = CreateProcessW(mkcl_OSstring_self(os_command_real_name),
			       mkcl_OSstring_self(os_command),
			       NULL,
			       NULL,
			       TRUE, /* bInheritHandles */
			       CREATE_NEW_CONSOLE, /* dwCreationFlags */
			       NULL,
			       os_raw_new_directory,
			       &StartUp,
			       &ProcInfo);
    }
  else
    success = CreateProcessW(NULL,
			     mkcl_OSstring_self(os_command),
			     NULL,
			     NULL,
			     TRUE, /* bInheritHandles */
			     CREATE_NEW_CONSOLE, /* dwCreationFlags */
			     NULL,
			     os_raw_new_directory,
			     &StartUp,
			     &ProcInfo);
  if (success == 0)
    mkcl_FEwin32_error(env,
		       "unable to create subprocess to execute command: ~A", 
		       1, command);

  if (!CloseHandle(ProcInfo.hThread))
    mkcl_FEwin32_error(env, "mkcl::run-command failed on CloseHandle", 0);

  if (!CloseHandle(child_stdin))
    mkcl_FEwin32_error(env, "mkcl::run-command failed on CloseHandle", 0);
  if (!CloseHandle(child_stdout_write))
    mkcl_FEwin32_error(env, "mkcl::run-command failed on CloseHandle", 0);
  if (!CloseHandle(child_stderr))
    mkcl_FEwin32_error(env, "mkcl::run-command failed on CloseHandle", 0);

  /* Recover child output. */
  output = read_command_output(env, child_stdout_read);

  {
    DWORD wait_val;
    
#if 0
    MKCL_LIBC_NO_INTR(env, wait_val = WaitForSingleObject(ProcInfo.hProcess, INFINITE));
#else
    do {
      MKCL_LIBC_Zzz(env, @':io', wait_val = WaitForSingleObjectEx(ProcInfo.hProcess, INFINITE, TRUE));
    } while (wait_val == WAIT_IO_COMPLETION);
#endif
    switch (wait_val)
      {
      case WAIT_OBJECT_0: break;
      default:
	mkcl_FEwin32_error(env, "mkcl::run-command failed on WaitForSingleObject", 0);
      }
    mk_mt_test_for_thread_shutdown(env);
  }

  if (!GetExitCodeProcess(ProcInfo.hProcess, &ExitCode))
    mkcl_FEwin32_error(env, "mkcl::run-command failed on GetExitCodeProcess", 0);

  if (!CloseHandle(child_stdout_read))
    mkcl_FEwin32_error(env, "mkcl::run-command failed on CloseHandle", 0);

  if (!CloseHandle(ProcInfo.hProcess))
    mkcl_FEwin32_error(env, "mkcl::run-command failed on CloseHandle", 0);

#ifdef WIN64
  { @(return MKCL_MAKE_FIXNUM(ExitCode) output); }
#else
  { @(return mkcl_make_unsigned_integer(env, ExitCode) output); }
#endif
@)


#elif defined(__unix)  /* MKCL_WINDOWS */

static void delete_pid_from_children(MKCL, pid_t pid)
{
  /* Delete child from the list of children. */
  volatile bool locked = false;
  
  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, (CHILDREN_LIST_LOCK(env), locked = TRUE));
    {
      mkcl_object children = mkcl_core.children;
      if (!mkcl_Null(children))
	{
	  mkcl_object child_pid = MKCL_CONS_CAR(children);
	  if ((MKCL_FIXNUMP(child_pid) && (mkcl_fixnum_to_word(child_pid) == pid))
	      || (MKCL_BIGNUMP(child_pid) && mpz_cmp_si(child_pid->big.big_num, pid)))
	    mkcl_core.children = MKCL_CONS_CDR(children);
	  else
	    {
	      mkcl_object previous = children;
	      children = MKCL_CONS_CDR(children);

	      mkcl_loop_for_on_unsafe(children) {
		mkcl_object child_pid = MKCL_CONS_CAR(children);

		if ((MKCL_FIXNUMP(child_pid) && (mkcl_fixnum_to_word(child_pid) == pid))
		    || (MKCL_BIGNUMP(child_pid) && mpz_cmp_si(child_pid->big.big_num, pid)))
		  {
		    MKCL_RPLACD(previous, MKCL_CONS_CDR(children));
		    break;
		  }
		previous = children;
	      } mkcl_end_loop_for_on;
	    }
	}
    }
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) CHILDREN_LIST_UNLOCK(env);
  } MKCL_UNWIND_PROTECT_END;
}

static void delete_pid_from_detached_children(pid_t pid)
{
  /* Delete child from the list of detached children. */
  mkcl_object detached_children = mkcl_core.detached_children;
  
  mkcl_loop_for_on_unsafe(detached_children) {
    mkcl_object child_pid = MKCL_CONS_CAR(detached_children);
    
    if ((MKCL_FIXNUMP(child_pid) && (mkcl_fixnum_to_word(child_pid) == pid))
	|| (MKCL_BIGNUMP(child_pid) && mpz_cmp_si(child_pid->big.big_num, pid)))
      {
	/* This splicing works because of the sentinel put at the end of the detached_children list. */
	MKCL_RPLACA(detached_children, MKCL_CONS_CAR(MKCL_CONS_CDR(detached_children)));
	MKCL_RPLACD(detached_children, MKCL_CONS_CDR(MKCL_CONS_CDR(detached_children)));
	break;
      }
  } mkcl_end_loop_for_on;
}


static mkcl_object read_command_output(MKCL, int parent_read)
{
  mkcl_object output = mk_cl_Cnil;
  mkcl_object out_tail = mk_cl_Cnil;
  bool at_eof = false;

  while (!at_eof)
    {
      char buffer[16 * 1024];
      int read_count;
      mkcl_object paragraph;

      MKCL_LIBC_Zzz(env, @':io', read_count = read(parent_read, buffer, sizeof(buffer) - sizeof(buffer[0])));
      mk_mt_test_for_thread_shutdown(env);
      switch (read_count)
	{
	case -1:
	  if (errno != EINTR)
	    mkcl_FElibc_error(env, "read_command_output failed on read().", 0);
	  break;
	case 0: at_eof = true; break;
	default:
	  buffer[read_count/sizeof(buffer[0])] = 0; /* mark end of string. */
	  paragraph = mkcl_cstring_to_string(env, buffer);
	  
	  /* accumulate output */
	  if (mkcl_Null(out_tail))
	    output = out_tail = MKCL_CONS(env, paragraph, mk_cl_Cnil);
	  else
	    {
	      mkcl_object this = MKCL_CONS(env, paragraph, mk_cl_Cnil);
	      
	      MKCL_RPLACD(out_tail, this);
	      out_tail = this;
	    }
	  break;
	}
    }
  return output;
}



static int my_exec_command(mkcl_char8 * cmd_real_name, mkcl_char8 * cmd_line)
{
  mkcl_char8 ch;
  int argc = 1;
  int cmd_line_end, i;
  int last_arg_index = 0;

  for (i = 0; cmd_line[i] == ' '; i++); /* swallow leading blanks. */
  for (; ch = cmd_line[i]; i++)
    {
      if (ch == ' ')
	{
	  cmd_line[i] = '\0'; /* slice it here */
	  argc++;
	  last_arg_index = i+1;
	  while ((ch = cmd_line[i+1]) == ' ') i++;
	  if (ch == '\0') argc--; /* trailing blanks do not count as an arg. */
	}
      else if (ch == '"')
	{
	  cmd_line[i] = ' '; /* quotes become spaces */
	  for (; ch = cmd_line[i+1]; i++)
	    {
	      if (ch == '"')
		{
		  cmd_line[i+1] = ' ';  /* quotes become spaces */
		  break;
		}
	      else if (ch == '\\')
		{
		  i++;
		  if (!(ch = cmd_line[i+1])) { i++; break; }
		}
	    }
	}
    }
  cmd_line_end = i;
  {
    mkcl_char8 * argv[argc + 1]; /* a VLA. */
    int j = 1;

    for (i = 0; cmd_line[i] == ' '; i++);
    argv[0] = &cmd_line[i];

    for (i = 0; i < last_arg_index; i++)
      if ((cmd_line[i] == '\0') && (j < argc))
	{
	  for (i++; cmd_line[i] == ' '; i++);
	  argv[j++] = &cmd_line[i];
	} 
    argv[argc] = NULL;

    if (cmd_real_name)
      return execv((char *) cmd_real_name, (char **) argv);
    else
      return execvp(argv[0], (char **) argv);
  }
}

@(defun mkcl::run-command (cmd_string directory &key real_name)
@
  static const mkcl_base_string_object(dummy_real_name, "");
  mkcl_object real_name_proxy = (mkcl_Null(real_name) ? (mkcl_object)&dummy_real_name : real_name);
  int parent_read;
  int child_stdin, child_stdout, child_stderr;
  int parent_to_child_in, parent_to_child_out;
  int fd[2];
  int status;
  pid_t pid;
  char msg[sizeof(intptr_t)];
  mkcl_object output = mk_cl_Cnil;
#if 0
  mkcl_object os_cmd = mkcl_string_to_OSstring(env, cmd_string);
  mkcl_object os_new_directory;
#else
  mkcl_dynamic_extent_OSstring(env, os_cmd, cmd_string);
  mkcl_dynamic_extent_OSstring(env, os_new_directory, directory);
  mkcl_dynamic_extent_OSstring(env, os_cmd_real_name, real_name_proxy);  
#endif
  char * os_raw_new_directory = mkcl_OSstring_self(os_new_directory);

  if (mkcl_OSstring_size(os_cmd) >= mkcl_core.arg_max)
    mkcl_FEerror(env, "Too long command line: ~S.", 1, cmd_string);

  /* prepare redirection of stdout (and stderr). */
  if (pipe(fd) < 0)
    mkcl_FElibc_error(env, "mkcl:run-command failed on pipe [output].", 0);
  parent_read = fd[0];
  child_stdout = fd[1];
  child_stderr = child_stdout;

  /* build synchronization pipe */
  if (pipe(fd) < 0)
    mkcl_FElibc_error(env, "mkcl:run-command failed on pipe [output].", 0);
  parent_to_child_in = fd[0];
  parent_to_child_out = fd[1];

  /* This code paragraph comes from the book
     "Advanced Programming in the UNIX Environment"
     by Richard Stevens, page 223.

     Redirection of stdin, stdout, stderr was added later.
  */
  if ((pid = fork()) < 0) { /* error */
    int saved_errno = errno;
    mkcl_safe_close(env, parent_to_child_in, mk_cl_Cnil);
    mkcl_safe_close(env, parent_to_child_out, mk_cl_Cnil);

    mkcl_safe_close(env, parent_read, mk_cl_Cnil);
    mkcl_safe_close(env, child_stdout, mk_cl_Cnil);

    mkcl_FElibc_error(env, "mkcl:run-command unable to fork subprocess to execute command: ~A", 1, cmd_string);
  } else if (pid == 0) { /* child */
    int rc;

    while (((rc = read(parent_to_child_out, msg, 1)) < 0) && (errno == EINTR));

    mkcl_safe_close(env, parent_to_child_in, mk_cl_Cnil);
    mkcl_safe_close(env, parent_to_child_out, mk_cl_Cnil);

    /* redirect stdin to /dev/null. */
    mkcl_safe_close(env, 0, mk_cl_Cnil);
    if ((child_stdin = open("/dev/null", O_RDONLY)) < 0)
      mkcl_FElibc_error(env, "mkcl:run-command child failed on open. [child_stdin].", 0);
    /* redirect stdout. */
    mkcl_safe_close(env, 1, mk_cl_Cnil);
    if (dup(child_stdout) == -1)
      mkcl_FElibc_error(env, "mkcl:run-command child failed on dup(child_stdout).", 0);;
    /* redirect stderr. */
    mkcl_safe_close(env, 2, mk_cl_Cnil);
    if (dup(child_stderr) == -1)
      mkcl_FElibc_error(env, "mkcl:run-command child failed on dup(child_stderr).", 0);
    
    mkcl_safe_close(env, child_stdout, mk_cl_Cnil);
    mkcl_safe_close(env, parent_read, mk_cl_Cnil);

    if (chdir(os_raw_new_directory))
      {
	fprintf(stderr, "\nMKCL: mkcl::run-command: chdir(%s): ", os_raw_new_directory);
      }
    else 
      {
#if 0
	execl("/bin/sh", "sh", "-c", mkcl_OSstring_self(os_cmd), (char *) 0);
#else
	if (mkcl_Null(real_name))
	  my_exec_command(NULL, mkcl_OSstring_self(os_cmd));
	else
	  my_exec_command(mkcl_OSstring_self(os_cmd_real_name), mkcl_OSstring_self(os_cmd));
#endif
	fprintf(stderr, "\nMKCL: mkcl::run-command: %s: ", mkcl_OSstring_self(os_cmd));
      }
    perror(NULL);
    fflush(stderr);
    _exit(127); /* execl failed! Let's report it like system() does. */
  } else { /* parent */
    int rc;

    {
      volatile bool locked = false;
      
      MKCL_UNWIND_PROTECT_BEGIN(env) {
	MKCL_LIBC_NO_INTR(env, (CHILDREN_LIST_LOCK(env), locked = TRUE));
	mkcl_core.children = mkcl_cons(env, mkcl_make_integer(env, pid), mkcl_core.children);
      } MKCL_UNWIND_PROTECT_EXIT {
	if (locked) CHILDREN_LIST_UNLOCK(env);
      } MKCL_UNWIND_PROTECT_END;
    }
    
    while ((rc = write(parent_to_child_in, "!", 1)) < 1)
      if ((rc == -1) && (errno != EINTR)) break;
    
    mkcl_safe_close(env, parent_to_child_in, mk_cl_Cnil);
    mkcl_safe_close(env, parent_to_child_out, mk_cl_Cnil);

    mkcl_safe_close(env, child_stdout, mk_cl_Cnil);

    /* Here we must read what the child wrote to its stdout. */
    output = read_command_output(env, parent_read);

    mkcl_safe_close(env, parent_read, mk_cl_Cnil);

#if 0
    while (((rc = waitpid(pid, &status, 0)) == -1) && (errno == EINTR));

    if ((rc == pid) && (WIFEXITED(status) || WIFSIGNALED(status)))
      delete_pid_from_children(env, pid);
    /* else this should probably be reported as a libc error result. */
#else
    for (;;)
      {
	int rc;

	MKCL_LIBC_Zzz(env, @':io', rc = waitpid(pid, &status, 0));
	mk_mt_test_for_thread_shutdown(env);
	if ((rc == pid) && (WIFEXITED(status) || WIFSIGNALED(status)))
	  break;
	else if ((rc == -1) && (errno != EINTR))
	  mkcl_FElibc_error(env, "mkcl:run-command failed on waitpid(), pid = ~S", 1, mkcl_make_integer(env, pid));
      }

    delete_pid_from_children(env, pid);
#endif
  } /* end of quote from Stevens. */

  if (WIFEXITED(status))
    { @(return MKCL_MAKE_FIXNUM(WEXITSTATUS(status)) output); }
  else if (WIFSIGNALED(status))
    { @(return mkcl_unix_signal_name(env, WTERMSIG(status))); }
  else
    { @(return mk_cl_Cnil output); }
@)

#endif /* defined(__unix) */

mkcl_object mk_mkcl_system (MKCL, mkcl_object cmd_string)
{
  /* On unix we ignore 'directory' since it plays
     no role in the executable search sequence.
     Everything depends on the PATH environment variable.
   */
  mkcl_dynamic_extent_OSstring(env, os_cmd, cmd_string);

#ifdef __unix
  if (mkcl_OSstring_size(os_cmd) >= mkcl_core.arg_max)
    mkcl_FEerror(env, "Too long command line: ~S.", 1, cmd_string);
#endif

  {
#ifdef MKCL_WINDOWS
    int code = _wsystem(mkcl_OSstring_self(os_cmd));
#else
    /* This call to 'system' has the side-effect, for its duration, of
       blocking SIGCHLD and ignoring both SIGINT and SIGQUIT! JCB */
    /* It also shares, between parent and child, all file descriptors
       (among others: stdin, stdout, stderr). JCB */
    int code = system(mkcl_OSstring_self(os_cmd));
#endif

    if (code == -1)
      mkcl_FElibc_error(env, "si::system unable to fork subprocess to execute command: ~A", 1, cmd_string);

#ifdef MKCL_WINDOWS
    { @(return MKCL_MAKE_FIXNUM(code)); }
#else
    if (WIFEXITED(code))
      { @(return MKCL_MAKE_FIXNUM(WEXITSTATUS(code))); }
    else if (WIFSIGNALED(code))
      { @(return mkcl_unix_signal_name(env, WTERMSIG(code))); }
    else
      { @(return mk_cl_Cnil); }
#endif
  }
}


mkcl_object
mk_mkcl_getpid(MKCL)
{
  mkcl_call_stack_check(env);
  @(return MKCL_MAKE_FIXNUM(getpid()));
}

pid_t mkcl_gettid(void)
{
#ifdef __linux
  return syscall(SYS_gettid);
#elif defined(MKCL_WINDOWS)
  return GetCurrentThreadId();
#else
  return getpid();
#endif
}

mkcl_object
mk_mkcl_gettid(MKCL)
{
  mkcl_call_stack_check(env);
  @(return MKCL_MAKE_FIXNUM(mkcl_gettid()));
}

mkcl_object
mk_mkcl_getuid(MKCL)
{
  mkcl_call_stack_check(env);
#if defined(MKCL_WINDOWS)
  /* GetUserName() followed by LookupAccountName(). JCB */
  @(return MKCL_MAKE_FIXNUM(0));
#else
  @(return mkcl_make_integer(env, getuid()));
#endif
}

mkcl_object
mk_mkcl_make_pipe(MKCL) /* Any user of this? JCB */ /* Without :element-type or :external-format it is useless anyway! */
{
  mkcl_object output;
  int fds[2], ret;
#if defined(MKCL_WINDOWS)
  ret = _pipe(fds, 4096, _O_BINARY);
#else
  ret = pipe(fds);
#endif
  if (ret < 0) {
    mkcl_FElibc_error(env, "si:make-pipe was unable to create pipe", 0);
    output = mk_cl_Cnil;
  } else {
    mkcl_object fake_in_name
      = mkcl_make_simple_base_string(env, "PIPE-READ-ENDPOINT");
    mkcl_object in = mkcl_make_stream_from_fd(env, fake_in_name,
					      fds[0], mkcl_smm_input, mk_cl_Cnil,
					      /* MKCL_STREAM_DEFAULT_FORMAT, */ /* 0, */
					      /* mk_cl_Cnil */ @':default');

    mkcl_object fake_out_name
      = mkcl_make_simple_base_string(env, "PIPE-WRITE-ENDPOINT");
    mkcl_object out = mkcl_make_stream_from_fd(env, fake_out_name,
					       fds[1], mkcl_smm_output, mk_cl_Cnil,
					       /* MKCL_STREAM_DEFAULT_FORMAT, */ /* 0, */
					       /* mk_cl_Cnil */ @':default');

    output = mk_cl_make_two_way_stream(env, in, out);
  }
  @(return output);
}

static mkcl_object build_unix_os_argv(MKCL, mkcl_object os_command, mkcl_object argv)
{
  mkcl_object os_argv_list = mkcl_list1(env, os_command);
  mkcl_object tail = os_argv_list;

  if (!MKCL_LISTP(argv)) mkcl_FEtype_error_list(env, argv);

  for (; MKCL_CONSP(argv); argv = MKCL_CONS_CDR(argv))
    {
      mkcl_object cons = mkcl_list1(env, mkcl_string_to_OSstring(env, MKCL_CONS_CAR(argv)));

      MKCL_RPLACD(tail, cons);
      tail = cons;
    }
  MKCL_RPLACD(tail, mkcl_list1(env, mk_cl_Cnil)); /* Put a NIL object at the end of the list. Unix needs it. */

  return mkcl_funcall2(env, @+'coerce', os_argv_list, @'vector');
}

@(defun mkcl::run-program-1 (command argv
                             &key
                             (input @':stream') (output @':stream') (error @'t')
                             (external_format @':default') (element_type @'character') (environment @'nil')
                             (directory @'nil') (search @'t') (wait @'t') (detached @'nil'))
  int parent_write = 0, parent_read = 0, parent_error = 0;
  mkcl_object stream_write;
  mkcl_object stream_read;
  mkcl_object stream_error;
  mkcl_object subprocess = mkcl_alloc_raw_process(env);
  mkcl_object exit_status = mk_cl_Cnil;
@
  /* 'environment' is the last keyword argument that we need to add. FIXME soon. */
  /* keyword arguments 'external-format' and 'element-type' are not used yet. FIXME soon. */
  subprocess->process.detached = mkcl_Null(detached) ? FALSE : TRUE;
  subprocess->process.command = command;
  subprocess->process.argv = argv;
  subprocess->process.ident = 0;
  subprocess->process.exit_code = -1;
  subprocess->process.plist = mk_cl_Cnil;
  subprocess->process.status = mk_cl_Cnil;
  subprocess->process.input = mk_cl_Cnil;
  subprocess->process.output = mk_cl_Cnil;
  subprocess->process.error = mk_cl_Cnil;  
  mk_si_set_finalizer(env, subprocess, mk_cl_Ct);
#if defined(MKCL_WINDOWS)
  {
    BOOL ok;
    STARTUPINFOW st_info;
    PROCESS_INFORMATION pr_info;
    HANDLE child_stdout, child_stdin, child_stderr;
    HANDLE current = GetCurrentProcess();
    SECURITY_ATTRIBUTES attr;
    mkcl_dynamic_extent_OSstring(env, os_command, command);
    

    /* Enclose each argument, as well as the file name
       in double quotes, to avoid problems when these
       arguments or file names have spaces */
    static const mkcl_base_string_object(format_control_string_obj, "~S~{ ~S~}");
    mkcl_object command_line = mk_cl_format(env, 4, mk_cl_Cnil, (mkcl_object) &format_control_string_obj, command, argv);

    attr.nLength = sizeof(SECURITY_ATTRIBUTES);
    attr.lpSecurityDescriptor = NULL;
    attr.bInheritHandle = TRUE;
    if (input == @':stream') {
      /* Creates a pipe that we can read from what the child
	 writes to it. We duplicate one end of the pipe
	 so that the child does not inherit it. */
      HANDLE tmp;
      ok = CreatePipe(&child_stdin, &tmp, &attr, 0);
      if (ok) {
	ok = DuplicateHandle(current, tmp, current,
			     &tmp, 0, FALSE,
			     DUPLICATE_CLOSE_SOURCE | DUPLICATE_SAME_ACCESS);
	if (ok) {
	  parent_write = _open_osfhandle((intptr_t) tmp, _O_WRONLY /*| _O_TEXT*/);
	  if (parent_write == -1)
	    mkcl_FElibc_error(env, "mkcl:run-program failed on open_osfhandle() for parent_write", 0);
	}
	else
	  mkcl_FEwin32_error(env, "mkcl:run-program failed on DuplicateHandle() for parent_write", 0);
      }
      else
	mkcl_FEwin32_error(env, "mkcl:run-program failed on CreatePipe() for child_stdin", 0);
    } else if (input == @'t') {
      /* The child inherits a duplicate of our input
	 handle. Creating a duplicate avoids problems when
	 the child closes it */
      mkcl_object input_stream = mkcl_symbol_value(env, @'*standard-input*');
      int stream_handle = mkcl_stream_to_handle(env, input_stream, 0);
      if (stream_handle >= 0)
	{
	  ok = DuplicateHandle(current,
			       (HANDLE) _get_osfhandle(stream_handle),
			       current,
			       &child_stdin,
			       0,
			       TRUE,
			       DUPLICATE_SAME_ACCESS);
	  if (!ok) mkcl_FEwin32_error(env, "mkcl:run-program failed on DuplicateHandle() for child_stdin", 0);
	}
      else {
	child_stdin = CreateFileW(L"NUL", GENERIC_READ,
				  FILE_SHARE_READ | FILE_SHARE_WRITE,
				  &attr, OPEN_EXISTING, 0, NULL);
	if (child_stdin == INVALID_HANDLE_VALUE)
	  mkcl_FEwin32_error(env, "mkcl:run-program failed to open NUL device for child_stdin", 0);
      }
    } else if (mkcl_Null(input)) {
      child_stdin = CreateFileW(L"NUL", GENERIC_READ,
				FILE_SHARE_READ | FILE_SHARE_WRITE,
				&attr, OPEN_EXISTING, 0, NULL);
      if (child_stdin == INVALID_HANDLE_VALUE)
	mkcl_FEwin32_error(env, "mkcl:run-program failed to open NUL device for child_stdin", 0);
    } else
      mkcl_FEerror(env, "mkcl:run-program, :input argument value invalid (~S), must be one of (:STREAM T NIL)", 1, input);

    if (output == @':stream') {
      /* Creates a pipe that we can write to and the
	 child reads from. We duplicate one end of the
	 pipe so that the child does not inherit it. */
      HANDLE tmp;
      ok = CreatePipe(&tmp, &child_stdout, &attr, 0);
      if (ok) {
	ok = DuplicateHandle(current, tmp, current,
			     &tmp, 0, FALSE,
			     DUPLICATE_CLOSE_SOURCE | DUPLICATE_SAME_ACCESS);
	if (ok) {
	  parent_read = _open_osfhandle((intptr_t) tmp, _O_RDONLY /*| _O_TEXT*/);
	  if (parent_read == -1)
	    mkcl_FElibc_error(env, "mkcl:run-program failed on open_osfhandle() for parent_read", 0);
	}
	else
	  mkcl_FEwin32_error(env, "mkcl:run-program failed on DuplicateHandle() for parent_read", 0);
      }
      else
	mkcl_FEwin32_error(env, "mkcl:run-program failed on CreatePipe() for child_stdout", 0);
    } else if (output == @'t') {
      /* The child inherits a duplicate of our output
	 handle. Creating a duplicate avoids problems when
	 the child closes it */
      mkcl_object output_stream = mkcl_symbol_value(env, @'*standard-output*');
      int stream_handle = mkcl_stream_to_handle(env, output_stream, 1);
      if (stream_handle >= 0)
	{
	  ok = DuplicateHandle(current,
			       (HANDLE) _get_osfhandle(stream_handle),
			       current, &child_stdout, 0, TRUE,
			       DUPLICATE_SAME_ACCESS);
	  if (!ok) mkcl_FEwin32_error(env, "mkcl:run-program failed on DuplicateHandle() for child_stdout", 0);
	}	  
      else {
	child_stdout = CreateFileW(L"NUL", GENERIC_WRITE,
				   FILE_SHARE_READ | FILE_SHARE_WRITE,
				   &attr, OPEN_EXISTING, 0, NULL);
	if (child_stdout == INVALID_HANDLE_VALUE)
	  mkcl_FEwin32_error(env, "mkcl:run-program failed to open NUL device for child_stdout", 0);
      }
    } else if (mkcl_Null(output)) {
      child_stdout = CreateFileW(L"NUL", GENERIC_WRITE,
				 FILE_SHARE_READ | FILE_SHARE_WRITE,
				 &attr, OPEN_EXISTING, 0, NULL);
      if (child_stdout == INVALID_HANDLE_VALUE)
	mkcl_FEwin32_error(env, "mkcl:run-program failed to open NUL device for child_stdout", 0);
    } else
      mkcl_FEerror(env, "mkcl:run-program, :output argument value invalid (~S), must be one of (:STREAM T NIL)", 1, output);

    if (error == @':stream') {
      /* Creates a pipe that we can write to and the
	 child reads from. We duplicate one end of the
	 pipe so that the child does not inherit it. */
      HANDLE tmp;
      ok = CreatePipe(&tmp, &child_stderr, &attr, 0);
      if (ok) {
	ok = DuplicateHandle(current, tmp, current,
			     &tmp, 0, FALSE,
			     DUPLICATE_CLOSE_SOURCE | DUPLICATE_SAME_ACCESS);
	if (ok) {
	  parent_error = _open_osfhandle((intptr_t) tmp, _O_RDONLY /*| _O_TEXT*/);
	  if (parent_error == -1)
	    mkcl_FElibc_error(env, "mkcl:run-program failed on open_osfhandle() for parent_error", 0);
	}
	else
	  mkcl_FEwin32_error(env, "mkcl:run-program failed on DuplicateHandle() for parent_error", 0);
      }
      else
	mkcl_FEwin32_error(env, "mkcl:run-program failed on CreatePipe() for child_stderr", 0);
    } else if (error == @':output') {
      /* The child inherits a duplicate of its own output handle.*/
      ok = DuplicateHandle(current, child_stdout, current,
			   &child_stderr, 0, TRUE,
			   DUPLICATE_SAME_ACCESS);
      if (!ok) mkcl_FEwin32_error(env, "mkcl:run-program failed on DuplicateHandle() for child_stderr", 0);
    } else if (error == @'t') {
      /* The child inherits a duplicate of our output
	 handle. Creating a duplicate avoids problems when
	 the child closes it */
      mkcl_object error_stream = mkcl_symbol_value(env, @'*error-output*');
      int stream_handle = mkcl_stream_to_handle(env, error_stream, 1);
      if (stream_handle >= 0)
	{
	  ok = DuplicateHandle(current,
			       (HANDLE) _get_osfhandle(stream_handle),
			       current, &child_stderr, 0, TRUE,
			       DUPLICATE_SAME_ACCESS);
	  if (!ok) mkcl_FEwin32_error(env, "mkcl:run-program failed on DuplicateHandle() for child_stderr", 0);
	}
      else {
	child_stderr = CreateFileW(L"NUL", GENERIC_WRITE,
				   FILE_SHARE_READ | FILE_SHARE_WRITE,
				   &attr, OPEN_EXISTING, 0, NULL);
	if (child_stderr == INVALID_HANDLE_VALUE)
	  mkcl_FEwin32_error(env, "mkcl:run-program failed to open NUL device for child_stderr", 0);
      }
    } else if (mkcl_Null(error)) {
      child_stderr = CreateFileW(L"NUL", GENERIC_WRITE,
				 FILE_SHARE_READ | FILE_SHARE_WRITE,
				 &attr, OPEN_EXISTING, 0, NULL);
      if (child_stderr == INVALID_HANDLE_VALUE)
	mkcl_FEwin32_error(env, "mkcl:run-program failed to open NUL device for child_stderr", 0);
    } else
      mkcl_FEerror(env, "mkcl:run-program, :error argument value invalid (~S), must be one of (:STREAM :OUTPUT T NIL)", 1, error);

    ZeroMemory(&st_info, sizeof(STARTUPINFO));
    st_info.cb = sizeof(STARTUPINFO);
    st_info.lpTitle = NULL; /* No window title, just exec name */
    st_info.dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW; /* Specify std{in,out,err} */
    st_info.wShowWindow = SW_HIDE;
    st_info.hStdInput = child_stdin;
    st_info.hStdOutput = child_stdout;
    st_info.hStdError = child_stderr;
    ZeroMemory(&pr_info, sizeof(PROCESS_INFORMATION));
    {
#if 0
      mkcl_object os_command_line = mkcl_string_to_OSstring(env, command_line);
#else
      mkcl_dynamic_extent_OSstring(env, os_command_line, command_line);
#endif
      mkcl_object os_new_directory;
      wchar_t * os_raw_new_directory = NULL;
      wchar_t * os_raw_command = NULL;

      if (!mkcl_Null(directory))
	{
	  os_new_directory = mkcl_string_to_OSstring(env, directory);
	  os_raw_new_directory = mkcl_OSstring_self(os_new_directory);
	}

      if (mkcl_Null(search))
	os_raw_command = mkcl_OSstring_self(os_command);


      ok = CreateProcessW(os_raw_command,
			  mkcl_OSstring_self(os_command_line),
			  NULL, /* lpProcessAttributes */
			  NULL, /* lpThreadAttributes */
			  TRUE, /* Inherit handles (for files) */
			  /*CREATE_NEW_CONSOLE |*/
			  0 /*(input == mk_cl_Ct || output == mk_cl_Ct || error == mk_cl_Ct ? 0 : CREATE_NO_WINDOW)*/,
			  NULL, /* Inherit environment */
			  os_raw_new_directory, /* Current directory */
			  &st_info, /* Startup info */
			  &pr_info); /* Process info */
    }

    /* Child handles must be closed in the parent process */
    /* otherwise the created pipes are never closed       */
    if (child_stdin) CloseHandle(child_stdin);
    if (child_stdout) CloseHandle(child_stdout);
    if (child_stderr) CloseHandle(child_stderr);
    if (ok) {
      DWORD exitcode;
      CloseHandle(pr_info.hThread);
      if (wait != mk_cl_Cnil) {
	DWORD wait_val;

#if 0
	MKCL_LIBC_NO_INTR(env, wait_val = WaitForSingleObject(pr_info.hProcess, INFINITE));
#else
	do {
	  MKCL_LIBC_Zzz(env, @':io', wait_val = WaitForSingleObjectEx(pr_info.hProcess, INFINITE, TRUE));
	} while (wait_val == WAIT_IO_COMPLETION);
#endif
	switch (wait_val)
	  {
	  case WAIT_OBJECT_0: break;
	  default:
	    mkcl_FEwin32_error(env, "mkcl:run-program failed on WaitForSingleObject() for subprocess", 0);
	  }
	mk_mt_test_for_thread_shutdown(env);

	if (GetExitCodeProcess(pr_info.hProcess, &exitcode)) {
	  if (STILL_ACTIVE != exitcode)
	    exit_status = MKCL_MAKE_FIXNUM(exitcode);
	  /* else should we go back and wait again for process completion? Is this case really possible? JCB */
	} else mkcl_FEwin32_error(env, "mkcl::run-program failed on GetExitCodeProcess", 0);
      }
      CloseHandle(pr_info.hProcess);
    } else {
      if (parent_write) 
	if (_close(parent_write))
	  mkcl_FElibc_error(env, "mkcl:run-program failed on _close(parent_write).", 0);
      if (parent_read) 
	if (_close(parent_read))
	  mkcl_FElibc_error(env, "mkcl:run-program failed on _close(parent_read).", 0);
      parent_write = 0;
      parent_read = 0;
      mkcl_FEerror(env, "mkcl:run-program could not spawn subprocess to run ~S.", 1, command);
    }
  }
#else /* def MKCL_WINDOWS */
  {
    pid_t child_pid;
    int rc;
    int child_stdin, child_stdout, child_stderr;
    int parent_to_child_in, parent_to_child_out;
    int child_to_parent_in, child_to_parent_out;
    char msg[sizeof(intptr_t)]; /* parent-child synchronization device */
    mkcl_object os_command = mkcl_string_to_OSstring(env, command);
    mkcl_object os_argv = build_unix_os_argv(env, os_command, argv);
    mkcl_object os_new_directory;
    char * os_raw_new_directory = NULL;

    if (input == @':stream') {
      int fd[2];
      if (pipe(fd) < 0)
	mkcl_FElibc_error(env, "mkcl:run-program failed on pipe [input].", 0);
      parent_write = fd[1];
      child_stdin = fd[0];
    } else {
      if (input == @'t') {
	mkcl_object input_stream = mkcl_symbol_value(env, @'*standard-input*');
	child_stdin = mkcl_stream_to_handle(env, input_stream, 0);
      }
      else if (mkcl_Null(input))
	child_stdin = -1;
      else
	mkcl_FEerror(env, "mkcl:run-program, :input argument value invalid (~S), must be one of (:STREAM T NIL)", 1, input);
      if (child_stdin >= 0) {
	if ((child_stdin = dup(child_stdin)) < 0)
	  mkcl_FElibc_error(env, "mkcl:run-program failed on dup(child_stdin).", 0);
      } else {
	if ((child_stdin = open("/dev/null", O_RDONLY)) < 0)
	  mkcl_FElibc_error(env, "mkcl:run-program failed on open. [child_stdin].", 0);
      }
    }
    if (output == @':stream') {
      int fd[2];
      if (pipe(fd) < 0)
	mkcl_FElibc_error(env, "mkcl:run-program failed on pipe [output].", 0);
      parent_read = fd[0];
      child_stdout = fd[1];
    } else {
      if (output == @'t') {
	mkcl_object output_stream = mkcl_symbol_value(env, @'*standard-output*');
	child_stdout = mkcl_stream_to_handle(env, output_stream, 1);
      }
      else if (mkcl_Null(output))
	child_stdout = -1;
      else
	mkcl_FEerror(env, "mkcl:run-program, :output argument value invalid (~S), must be one of (:STREAM T NIL)", 1, output);
      if (child_stdout >= 0) {
	if ((child_stdout = dup(child_stdout)) < 0)
	  mkcl_FElibc_error(env, "mkcl:run-program failed on dup(child_stdout).", 0);
      } else {
	if ((child_stdout = open("/dev/null", O_WRONLY)) < 0)
	  mkcl_FElibc_error(env, "mkcl:run-program failed on open. [child_stdout].", 0);
      }
    }
    if (error == @':stream')
      {
	int fd[2];
	if (pipe(fd) < 0)
	  mkcl_FElibc_error(env, "mkcl:run-program failed on pipe [error].", 0);
	parent_error = fd[0];
	child_stderr = fd[1];
      }
    else
      {
	if (error == @':output') {
	  child_stderr = child_stdout;
	} else if (error == @'t') {
	  mkcl_object error_stream = mkcl_symbol_value(env, @'*error-output*');
	  child_stderr = mkcl_stream_to_handle(env, error_stream, 1);
	} else if (mkcl_Null(error)) {
	  child_stderr = -1;
	} else
	  mkcl_FEerror(env, "mkcl:run-program, :error argument value invalid (~S), must be one of (:STREAM :OUTPUT T NIL)", 1, error);

	if (child_stderr < 0) {
	  if ((child_stderr = open("/dev/null", O_WRONLY)) < 0)
	    mkcl_FElibc_error(env, "mkcl:run-program failed on open. [child_stderr].", 0);
	} else {
	  if ((child_stderr = dup(child_stderr)) < 0)
	    mkcl_FElibc_error(env, "mkcl:run-program failed on dup(child_stderr).", 0);
	}
      }
    {
      int fd[2];
      if (pipe(fd) < 0)
	mkcl_FElibc_error(env, "mkcl:run-program failed on pipe [error].", 0);
      parent_to_child_in = fd[0];
      parent_to_child_out = fd[1];

      if (pipe(fd) < 0)
	mkcl_FElibc_error(env, "mkcl:run-program failed on pipe [error].", 0);
      child_to_parent_in = fd[0];
      child_to_parent_out = fd[1];
    }

    if (!mkcl_Null(directory))
      {
	os_new_directory = mkcl_string_to_OSstring(env, directory);
	os_raw_new_directory = mkcl_OSstring_self(os_new_directory);
      }
    
    child_pid = fork();
    if (child_pid == 0)
      {	/* Child */
	size_t j;
	const size_t exec_argc = os_argv->vector.fillp;
	char * exec_argv[exec_argc]; /* a VLA. */

	while ((rc = write(child_to_parent_in, "?", 1)) < 1)
	  if ((rc == -1) && (errno != EINTR)) break;

	while (((rc = read(parent_to_child_out, msg, 1)) < 0) && (errno == EINTR));

	mkcl_safe_close(env, parent_to_child_in, mk_cl_Cnil);
	mkcl_safe_close(env, parent_to_child_out, mk_cl_Cnil);
	mkcl_safe_close(env, child_to_parent_in, mk_cl_Cnil);
	mkcl_safe_close(env, child_to_parent_out, mk_cl_Cnil);

	mkcl_safe_close(env, 0, mk_cl_Cnil);
	if (dup(child_stdin) == -1)
	  mkcl_FElibc_error(env, "mkcl:run-program child failed on dup(child_stdin).", 0);
	if (parent_write) mkcl_safe_close(env, parent_write, mk_cl_Cnil);
	mkcl_safe_close(env, 1, mk_cl_Cnil);
	if (dup(child_stdout) == -1)
	  mkcl_FElibc_error(env, "mkcl:run-program child failed on dup(child_stdout).", 0);;
	if (parent_read) mkcl_safe_close(env, parent_read, mk_cl_Cnil);
	mkcl_safe_close(env, 2, mk_cl_Cnil);
	if (dup(child_stderr) == -1)
	  mkcl_FElibc_error(env, "mkcl:run-program child failed on dup(child_stderr).", 0);
	if (parent_error) mkcl_safe_close(env, parent_error, mk_cl_Cnil);

	for (j = 0; j < exec_argc; j++) {
	  mkcl_object arg = os_argv->vector.self.t[j];
	  if (mkcl_Null(arg)) {
	    exec_argv[j] = NULL;
	  } else {
	    exec_argv[j] = mkcl_OSstring_self(arg);
	  }
	}

	if (os_raw_new_directory && chdir(os_raw_new_directory))
	  {
	    fprintf(stderr, "\nMKCL: si::run-program: chdir(%s): ", os_raw_new_directory);
	  }
	else 
	  {
	    if (mkcl_Null(search))
	      execv(mkcl_OSstring_self(os_command), exec_argv);
	    else
	      execvp(mkcl_OSstring_self(os_command), exec_argv);
	    fprintf(stderr, "\nMKCL: mkcl::run-command: %s: ", mkcl_OSstring_self(os_command));
	  }
	/* at this point exec has failed */
	perror(NULL);
	fflush(stderr);
	_exit(127); /* We have nothing else left to do but to terminate this child process. */
      } 
    else if (child_pid > 0) 
      {	/* Parent */
	while (((rc = read(child_to_parent_out, msg, 1)) < 0) && (errno == EINTR))
	  mk_mt_test_for_thread_shutdown(env);

	subprocess->process.ident = child_pid;
	subprocess->process.status = @':running';

	if (mkcl_Null(detached))
	  {
	    volatile bool locked = false;

	    MKCL_UNWIND_PROTECT_BEGIN(env) {
	      MKCL_LIBC_NO_INTR(env, (CHILDREN_LIST_LOCK(env), locked = TRUE));
	      mkcl_core.children = mkcl_cons(env, mkcl_make_integer(env, child_pid), mkcl_core.children);
	    } MKCL_UNWIND_PROTECT_EXIT {
	      if (locked) CHILDREN_LIST_UNLOCK(env);
	    } MKCL_UNWIND_PROTECT_END;

	    
	    while ((rc = write(parent_to_child_in, "!", 1)) < 1)
	      if ((rc == -1) && (errno != EINTR)) break;
	      else mk_mt_test_for_thread_shutdown(env);
	    
	    mkcl_safe_close(env, child_stdin, mk_cl_Cnil);
	    mkcl_safe_close(env, child_stdout, mk_cl_Cnil);
	    mkcl_safe_close(env, child_stderr, mk_cl_Cnil);
	    mkcl_safe_close(env, parent_to_child_in, mk_cl_Cnil);
	    mkcl_safe_close(env, parent_to_child_out, mk_cl_Cnil);
	    mkcl_safe_close(env, child_to_parent_in, mk_cl_Cnil);
	    mkcl_safe_close(env, child_to_parent_out, mk_cl_Cnil);
	    
	    
	    if (wait != mk_cl_Cnil) {
	      int status;
#if 0
	      while (((rc = waitpid(child_pid, &status, 0)) == -1) && (errno == EINTR));
	      if (rc == -1)
		mkcl_FElibc_error(env, "mkcl:run-program failed on waitpid().", 0);
	      else if (WIFEXITED(status))
		exit_status = MKCL_MAKE_FIXNUM(WEXITSTATUS(status));
	      else if (WIFSIGNALED(status))
		exit_status = mkcl_unix_signal_name(env, WTERMSIG(status));
	      else
		exit_status = mk_cl_Cnil;
	      
	      if ((rc == child_pid) && (WIFEXITED(status) || WIFSIGNALED(status)))
		{
		  delete_pid_from_children(env, child_pid);
		  subprocess->process.status = @':exited';
		  subprocess->process.exit_code = status;
		}
#else
	      for (;;)
		{
		  int rc;
		  MKCL_LIBC_Zzz(env, @':io', rc = waitpid(child_pid, &status, 0));
		  mk_mt_test_for_thread_shutdown(env);
		  if ((rc == child_pid) && (WIFEXITED(status) || WIFSIGNALED(status)))
		    break;
		  else if ((rc == -1) && (errno != EINTR))
		    mkcl_FElibc_error(env, "mkcl:run-program failed on waitpid(), pid = ~S", 1, mkcl_make_integer(env, child_pid));
		}
	      
	      delete_pid_from_children(env, child_pid);
	      subprocess->process.status = @':exited';
	      subprocess->process.exit_code = status;
	      if (WIFEXITED(status))
		exit_status = MKCL_MAKE_FIXNUM(WEXITSTATUS(status));
	      else if (WIFSIGNALED(status))
		exit_status = mkcl_unix_signal_name(env, WTERMSIG(status));
	      else
		exit_status = mk_cl_Cnil;
#endif	  
	    }
	  }
	else
	  { /* detached */
	    volatile bool locked = false;

	    MKCL_UNWIND_PROTECT_BEGIN(env) {
	      MKCL_LIBC_NO_INTR(env, (CHILDREN_LIST_LOCK(env), locked = TRUE));
	      mkcl_core.detached_children = mkcl_cons(env, mkcl_make_integer(env, child_pid), mkcl_core.detached_children);
	    } MKCL_UNWIND_PROTECT_EXIT {
	      if (locked) CHILDREN_LIST_UNLOCK(env);
	    } MKCL_UNWIND_PROTECT_END;
	    
	    while ((rc = write(parent_to_child_in, "!", 1)) < 1)
	      if ((rc == -1) && (errno != EINTR)) break;
	      else mk_mt_test_for_thread_shutdown(env);
	    
	    mkcl_safe_close(env, child_stdin, mk_cl_Cnil);
	    mkcl_safe_close(env, child_stdout, mk_cl_Cnil);
	    mkcl_safe_close(env, child_stderr, mk_cl_Cnil);
	    mkcl_safe_close(env, parent_to_child_in, mk_cl_Cnil);
	    mkcl_safe_close(env, parent_to_child_out, mk_cl_Cnil);
	    mkcl_safe_close(env, child_to_parent_in, mk_cl_Cnil);
	    mkcl_safe_close(env, child_to_parent_out, mk_cl_Cnil);

	    exit_status = @':detached';
	  }
      }
    else if (child_pid < 0)
      { /* Error */
	int fork_errno = errno;

	mkcl_safe_close(env, child_stdin, mk_cl_Cnil);
	mkcl_safe_close(env, child_stdout, mk_cl_Cnil);
	mkcl_safe_close(env, child_stderr, mk_cl_Cnil);
	mkcl_safe_close(env, parent_to_child_in, mk_cl_Cnil);
	mkcl_safe_close(env, parent_to_child_out, mk_cl_Cnil);
	mkcl_safe_close(env, child_to_parent_in, mk_cl_Cnil);
	mkcl_safe_close(env, child_to_parent_out, mk_cl_Cnil);

	if (parent_write) mkcl_safe_close(env, parent_write, mk_cl_Cnil);
	if (parent_read) mkcl_safe_close(env, parent_read, mk_cl_Cnil);
	if (parent_error) mkcl_safe_close(env, parent_error, mk_cl_Cnil);
	parent_write = 0;
	parent_read = 0;
	parent_error = 0;
	errno = fork_errno;
	mkcl_FElibc_error(env, "mkcl:run-program ould not spawn subprocess to run ~S.", 1, command);
      }
  }
#endif  /* def MKCL_WINDOWS */
  if (parent_write > 0) {
    stream_write = mkcl_make_stream_from_fd(env, command, parent_write,
					    mkcl_smm_output, mk_cl_Cnil,
					    /* MKCL_STREAM_DEFAULT_FORMAT, mk_cl_Ct */
					    /* MKCL_STREAM_TEXT, */ @':default');
  } else {
    parent_write = 0;
    stream_write = mkcl_core.null_stream;
  }
  subprocess->process.input = stream_write;
  if (parent_read > 0) {
    stream_read = mkcl_make_stream_from_fd(env, command, parent_read,
					   mkcl_smm_input, mk_cl_Cnil,
					   /* MKCL_STREAM_DEFAULT_FORMAT, mk_cl_Ct */
					   /* MKCL_STREAM_TEXT, */ @':default');
  } else {
    parent_read = 0;
    stream_read = mkcl_core.null_stream;
  }
  subprocess->process.output = stream_read;
  if (parent_error > 0) {
    stream_error = mkcl_make_stream_from_fd(env, command, parent_error,
					    mkcl_smm_input, mk_cl_Cnil,
					    /* MKCL_STREAM_DEFAULT_FORMAT, mk_cl_Ct */
					    /* MKCL_STREAM_TEXT, */ @':default');
  } else {
    parent_error = 0;
    stream_error = mkcl_core.null_stream;
  }
  subprocess->process.error = stream_error;
  @(return ((parent_read || parent_write)
	    ? mk_cl_make_two_way_stream(env, stream_read, stream_write)
	    : mk_cl_Cnil)
    subprocess
    exit_status);
@)


void mkcl_finalize_process(MKCL, mkcl_object proc)
{
  mk_mkcl_process_status(env, proc);
#if __unix
  delete_pid_from_children(env, proc->process.ident);
#elif defined(MKCL_WINDOWS)
  if (proc->process.ident)
    {
      CloseHandle(proc->process.ident);
      proc->process.ident = NULL;
    }
#endif
#if 0 /* We cannot know if the user sampled these stream therefore we cannot close them. */ /* stream finalizer closes anyway! */
  if (!mkcl_Null(proc->process.input)) mk_cl_close(env, 1, proc->process.input);
  if (!mkcl_Null(proc->process.output)) mk_cl_close(env, 1, proc->process.output);
  if (!mkcl_Null(proc->process.error)) mk_cl_close(env, 1, proc->process.error);
#endif
}


mkcl_object mk_mkcl_process_command(MKCL, mkcl_object proc)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(proc) != mkcl_t_process)
    mkcl_FEwrong_type_argument(env, @'mkcl::process', proc);
  @(return proc->process.command);
}

mkcl_object mk_mkcl_process_argv(MKCL, mkcl_object proc)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(proc) != mkcl_t_process)
    mkcl_FEwrong_type_argument(env, @'mkcl::process', proc);
  @(return proc->process.argv);
}

mkcl_object mk_mkcl_process_id(MKCL, mkcl_object proc)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(proc) != mkcl_t_process)
    mkcl_FEwrong_type_argument(env, @'mkcl::process', proc);
  else if (proc->process.detached)
    { @(return @':detached'); }

#if defined(MKCL_WINDOWS) 
# if defined(__MINGW32__) && !(_WIN32_WINNT >= 0x0501) /* Requires WinXP SP1 or later. */
  @(return mk_cl_Cnil);
# else
  @(return mkcl_make_integer(env, GetProcessId(proc->process.ident)));
# endif
#else
  @(return mkcl_make_integer(env, proc->process.ident));
#endif
}


mkcl_object mk_mkcl_process_input(MKCL, mkcl_object proc)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(proc) != mkcl_t_process)
    mkcl_FEwrong_type_argument(env, @'mkcl::process', proc);
  @(return proc->process.input);
}

mkcl_object mk_mkcl_process_output(MKCL, mkcl_object proc)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(proc) != mkcl_t_process)
    mkcl_FEwrong_type_argument(env, @'mkcl::process', proc);
  @(return proc->process.output);
}

mkcl_object mk_mkcl_process_error(MKCL, mkcl_object proc)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(proc) != mkcl_t_process)
    mkcl_FEwrong_type_argument(env, @'mkcl::process', proc);
  @(return proc->process.error);
}

mkcl_object mk_mkcl_process_status(MKCL, mkcl_object proc)
{
  mkcl_os_process_t child_pid;

  mkcl_call_stack_check(env);
  if (mkcl_type_of(proc) != mkcl_t_process)
    mkcl_FEwrong_type_argument(env, @'mkcl::process', proc);
  else if (proc->process.detached)
    { @(return @':detached'); }

#if __unix
  if (proc->process.status != @':exited')
    {
      int status, rc;

      child_pid = proc->process.ident;
      do
	{ MKCL_LIBC_Zzz(env, @':io', rc = waitpid(child_pid, &status, WNOHANG|WUNTRACED)); }
      while ((rc == -1) && (errno == EINTR));
      mk_mt_test_for_thread_shutdown(env);
      if (rc == -1)
	mkcl_FElibc_error(env, "si:process-status failed on waitpid().", 0);
      else if ((rc == child_pid) && (WIFEXITED(status) || WIFSIGNALED(status)))
	{
	  delete_pid_from_children(env, child_pid);
	  proc->process.status = @':exited';
	  proc->process.exit_code = status;
	}
    }
#elif defined(MKCL_WINDOWS)
  if (proc->process.status != @':exited')
    {
      DWORD exitcode;

      child_pid = proc->process.ident;
      if (GetExitCodeProcess(child_pid, &exitcode)) {
	if (STILL_ACTIVE != exitcode)
	  {
	    proc->process.status = @':exited';
	    proc->process.exit_code = exitcode;
	  }
      } else mkcl_FEwin32_error(env, "mkcl::run-program failed on GetExitCodeProcess", 0);
    }
#endif
  
  @(return proc->process.status);
}

mkcl_object mk_mkcl_process_exit_code(MKCL, mkcl_object proc)
{
  mkcl_exit_code_t exit_code;

  mkcl_call_stack_check(env);
  if (mkcl_type_of(proc) != mkcl_t_process)
    mkcl_FEwrong_type_argument(env, @'mkcl::process', proc);
  if (proc->process.status != @':exited')
    mk_mkcl_process_status(env, proc);
  else if (proc->process.detached)
    { @(return @':detached'); }

  exit_code = proc->process.exit_code;

#if __unix
  if (WIFEXITED(exit_code))
    { @(return MKCL_MAKE_FIXNUM(WEXITSTATUS(exit_code))); }
  else if (WIFSIGNALED(exit_code))
    { @(return mkcl_unix_signal_name(env, WTERMSIG(exit_code))); }
  else
    { @(return mk_cl_Cnil); }
#else
  @(return MKCL_MAKE_FIXNUM(exit_code));
#endif
}

mkcl_object mk_mkcl_process_p(MKCL, mkcl_object proc)
{
  @(return ((mkcl_type_of(proc) != mkcl_t_process) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object mk_mkcl_process_plist(MKCL, mkcl_object proc)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(proc) != mkcl_t_process)
    mkcl_FEwrong_type_argument(env, @'mkcl::process', proc);
  @(return proc->process.plist);
}

mkcl_object mk_mkcl_set_process_plist(MKCL, mkcl_object proc, mkcl_object plist)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(proc) != mkcl_t_process)
    mkcl_FEwrong_type_argument(env, @'mkcl::process', proc);
  @(return (proc->process.plist = plist));
}



mkcl_object mk_mkcl_join_process(MKCL, mkcl_object proc)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(proc) != mkcl_t_process)
    mkcl_FEwrong_type_argument(env, @'mkcl::process', proc);

  if (proc->process.status == @':exited')
    return mk_mkcl_process_exit_code(env, proc);
  else if (proc->process.detached)
    { @(return @':detached'); }

#if __unix
  mkcl_os_process_t pid = proc->process.ident;
  mkcl_exit_code_t exit_code;

  for (;;)
    {
      int rc;
      MKCL_LIBC_Zzz(env, @':io', rc = waitpid(pid, &exit_code, 0));
      mk_mt_test_for_thread_shutdown(env);
      if ((rc == pid) && (WIFEXITED(exit_code) || WIFSIGNALED(exit_code)))
	break;
      else if ((rc == -1) && (errno != EINTR))
	mkcl_FElibc_error(env, "mkcl:join-process failed on waitpid(), pid = ~S", 1, mkcl_make_integer(env, pid));
    }

  delete_pid_from_children(env, pid);
  proc->process.status = @':exited';
  proc->process.exit_code = exit_code;
  if (WIFEXITED(exit_code))
    { @(return MKCL_MAKE_FIXNUM(WEXITSTATUS(exit_code))); }
  else if (WIFSIGNALED(exit_code))
    { @(return mkcl_unix_signal_name(env, WTERMSIG(exit_code))); }
  else
    { @(return mk_cl_Cnil); }

#elif defined(MKCL_WINDOWS)
  mkcl_os_process_t pid = proc->process.ident;
  mkcl_exit_code_t exit_code;

 WAIT_AGAIN:
  {
    DWORD wait_val;
    
#if 0
    MKCL_LIBC_NO_INTR(env, wait_val = WaitForSingleObject(pid, INFINITE));
#else
    do {
      MKCL_LIBC_Zzz(env, @':io', wait_val = WaitForSingleObjectEx(pid, INFINITE, TRUE));
    } while (wait_val == WAIT_IO_COMPLETION);
#endif
    switch (wait_val)
      {
      case WAIT_OBJECT_0: break;
      default:
	mkcl_FEwin32_error(env, "mkcl:join-process failed on WaitForSingleObject()", 0);
      }
    mk_mt_test_for_thread_shutdown(env);
  }

  if (GetExitCodeProcess(pid, &exit_code)) {
    if (STILL_ACTIVE != exit_code)
      {
	proc->process.exit_code = exit_code;
	proc->process.status = @':exited';
      }
    else
      goto WAIT_AGAIN;
  } else mkcl_FEwin32_error(env, "mkcl:join-process failed on GetExitCodeProcess()", 0);

  @(return mkcl_make_integer(env, proc->process.exit_code));
#endif

}


@(defun mkcl::terminate-process (proc &key force)
@
  if (mkcl_type_of(proc) != mkcl_t_process)
    mkcl_FEwrong_type_argument(env, @'mkcl::process', proc);

  if (proc->process.status == @':exited' /* || proc->process.detached */)
    { @(return mk_cl_Cnil); }

#if __unix
  if (mkcl_Null(force))
    {
      if (kill(proc->process.ident, SIGTERM))
	mkcl_FElibc_error(env, "mkcl:terminate-process failed on kill().", 0);
    }
  else
    if (kill(proc->process.ident, SIGKILL))
      mkcl_FElibc_error(env, "mkcl:terminate-process failed on kill().", 0);

#elif defined(MKCL_WINDOWS)
  if (!TerminateProcess(proc->process.ident, -1))
    mkcl_FEwin32_error(env, "mkcl:terminate-process failed on TerminateProcess()", 0);
#endif

  @(return mk_cl_Cnil);  
@)

mkcl_object mk_mkcl_detach_process(MKCL, mkcl_object proc)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(proc) != mkcl_t_process)
    mkcl_FEwrong_type_argument(env, @'mkcl::process', proc);
#if __unix
  mkcl_os_process_t id = proc->process.ident;

  {
    volatile bool locked = false;

    MKCL_UNWIND_PROTECT_BEGIN(env) {
      MKCL_LIBC_NO_INTR(env, (CHILDREN_LIST_LOCK(env), locked = TRUE));
      mkcl_core.detached_children = mkcl_cons(env, mkcl_make_integer(env, id), mkcl_core.detached_children);
    } MKCL_UNWIND_PROTECT_EXIT {
      if (locked) CHILDREN_LIST_UNLOCK(env);
    } MKCL_UNWIND_PROTECT_END;
  }
  if (mk_mkcl_process_status(env, proc) == @':exited') /* proc may have exited before detached_children was updated. */
    delete_pid_from_detached_children(id);

  delete_pid_from_children(env, id);

#endif
  proc->process.detached = TRUE;
  @(return proc);
}

mkcl_object mk_mkcl_process_detached_p(MKCL, mkcl_object proc)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(proc) != mkcl_t_process)
    mkcl_FEwrong_type_argument(env, @'mkcl::process', proc);

  @(return (proc->process.detached ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object mk_si_list_all_children(MKCL) /* debug JCB */
{
  @(return mkcl_core.children mkcl_core.detached_children);
}


#ifndef MKCL_WINDOWS
# include <sys/utsname.h>
#endif

mkcl_object mk_si_uname(MKCL)
{
  mkcl_call_stack_check(env);
#ifdef MKCL_WINDOWS
  @(return);
#else
  mkcl_object output;
  struct utsname aux;
  int rc;

  MKCL_LIBC_NO_INTR(env, rc = uname(&aux));
  if (rc < 0)
    { 
      mkcl_FElibc_error(env, "uname() failed.", 0);
      @(return);
    }
  else
    {
      @(return
	mkcl_make_base_string_copy(env, aux.sysname)
	mkcl_make_base_string_copy(env, aux.nodename)
	mkcl_make_base_string_copy(env, aux.release)
	mkcl_make_base_string_copy(env, aux.version)
	mkcl_make_base_string_copy(env, aux.machine));
    }
#endif
}


void mkcl_init_unixsys(MKCL)
{
#ifdef __unix
  const pthread_mutexattr_t * const mutexattr = mkcl_normal_mutexattr;

  if (pthread_mutex_init(&children_list_lock, mutexattr))
    mkcl_FElibc_error(env, "mkcl_init_unixsys failed on pthread_mutex_init of children list lock.", 0);
#endif
}

void mkcl_clean_up_unixsys(MKCL)
{ /* Best effort only. We cannot raise an exception from here. */
#ifdef __unix
  (void) pthread_mutex_destroy(&children_list_lock);
#endif
}

