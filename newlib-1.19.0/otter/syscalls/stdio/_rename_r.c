#include <otter/multiotter_builtins.h>
#include <otter/otter_fs.h>

#include <stdio.h>
#include <unistd.h>
#include <errno.h>

/*
 With the options we give it, newlib #defines _rename_r to rename. However,
 while other functions for which newlib does that are then left undefined,
 rename is (for some reason) defined to call _rename_r, which means it
 recursively calls itself.

 newlib *does* have an implementation of _rename_r, in
 newlib/libc/reent/renamer.c . Depending on a macro, it either calls _rename
 (which the underlying system is assumed to have) or uses link and unlink, but
 the latter implementation is not correct. */

/* TODO: This only handles the case of renaming a file when no errors would
   occur. Other cases are not currently handled properly (including renaming
   directories, if old and new are the same path, if there are errors, ...). */
int _rename_r(struct _reent *ignored, const char *old_path, const char *new_path) {
	struct __otter_fs_dnode *old_dir, *new_dir;
	char *old_filename, *new_filename;
	struct __otter_fs_inode	*old_inode;

	// Find the old (target) directory and inode
	old_dir = find_filename_and_dnode(old_path, &old_filename);
	if (!old_dir) { return -1; }
	old_inode = __otter_fs_find_inode_in_dir(old_filename, old_dir);
	if (!old_inode) { return -1; }

	// Find the new directory
	new_dir = find_filename_and_dnode(new_path, &new_filename);
	if (!new_dir) { return -1; }

	// Atomically rename the file
	__otter_multi_begin_atomic();

  /* Unlink the new file, if it exists. If it doesn't exist, this will return an error, but we can ignore it. */
  __otter_fs_unlink_in_dir(new_filename, new_dir);
  /* Link the new filename to old_inode */
  __otter_fs_link_file(new_filename, old_inode, new_dir);
  /* Unlink the old file */
  __otter_fs_unlink_in_dir(old_filename, old_dir);

	__otter_multi_end_atomic();

	return 0;
}
