#!/bin/sh

#
# The original parts of this script came from SBCL 1.3.9 and is believed to be in the public domain.
#

#set -x # debug

git_available_p() {
    # Check that (1) we have git (2) this is a git tree.
    if ( command -v git >/dev/null && git describe >/dev/null 2>/dev/null )
    then
        echo "ok"
    else
        echo ""
    fi
}

if [ -z "$(git_available_p)" ]
then
    # Relase tarball.
    echo "(defun mkcl::git-describe-this-mkcl () nil)"
    exit 0
fi
# Build it.
version_head=`git rev-parse HEAD`
if grep -q "ref: refs/heads/.*" .git/HEAD > /dev/null 2>&1
then
    version_branchname=`cut -d / -f 3- < .git/HEAD`
else
    # Detached head.
    version_branchname="HEAD"
fi
if [ -z "$MKCL_BUILDING_RELEASE_FROM" ]
then
    if [ "`git rev-list HEAD --not origin/master`" = '' ]
    then
        # If origin/master contains all the commits on current
        # branch, use current head as the root instead.
        version_root="$version_branchname"
    else
        version_root="origin/master"
    fi
else
    version_root="$MKCL_BUILDING_RELEASE_FROM"
fi
version_base=`git rev-parse "$version_root"`
version_tag=`git describe --tags --match="v*" --abbrev=0 $version_base`
version_release=`echo $version_tag | sed -e 's/v//' | sed -e 's/_/\./g'`
# Using wc -l instead of --count argument to rev-list because
# pre-1.7.2 Gits are still common out in the wilderness.
version_n_root=`git rev-list $version_base --not $version_tag | wc -l | sed -e 's/[ \t]//g'`
version_n_branch=`git rev-list HEAD --not $version_base | wc -l | sed -e 's/[ \t]//g'`
if [ -z "$NO_GIT_HASH_IN_VERSION" ]
then
    version_hash="-`git rev-parse --short $version_head`"
else
    version_hash=""
fi
if git diff HEAD --no-ext-diff --quiet --exit-code
then
    version_dirty=""
else
    version_dirty="-dirty"
fi
# Now that we have all the pieces, put them together.
if [ "$version_base" = "$version_head" ]
then
    if [ "0" = "$version_n_root" ]
    then
        printf "(defun mkcl::git-describe-this-mkcl () \"%s%s\")\n" \
            $version_release $version_dirty
    else
        printf "(defun mkcl::git-describe-this-mkcl () \"%s.%s%s%s\")\n" \
            $version_release $version_n_root \
            $version_hash $version_dirty
    fi
else
    printf "(defun mkcl::git-describe-this-mkcl () \"%s.%s.%s.%s%s%s\")\n" \
        $version_release $version_n_root \
        $version_branchname $version_n_branch \
        $version_hash $version_dirty
fi
