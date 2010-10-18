#include <sys/mman.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <__otter/otter_fs.h>
#include <unistd.h>

/* mmap memory maps a file (or other sometimes other things that we don't
 * support) We have no support for memory protection as mmap understands it, so
 * by the spec mmap should always fail. Since this is not really an option we 
 * check the protections at the creation of the mapping, but cannot enforce
 * them later.
 */
void *__otter_libc_mmap(void *start, size_t len, int prot, int flags, int fd, off_t off)
{
	if(len == 0)
	{
		errno = EINVAL;
		return MAP_FAILED;
	}
	
	if(flags & MAP_FIXED) /* mapping to a particular memory address is not supported */
	{
		__ASSERT(0);
		return MAP_FAILED;
	}
	
	if(!((flags & MAP_PRIVATE)^(flags & MAP_SHARED))) /* must be either fixed or private */
	{
		errno = EINVAL;
		return MAP_FAILED;
	}
	
	long pagesize = sysconf(_SC_PAGE_SIZE);
	if(!(off % pagesize == 0)) /* off must corrispond to an integer multiple of pages */
	{
		errno = EINVAL;
		return MAP_FAILED;
	}
	
	struct __otter_fs_open_file_table_entry* open_file = get_open_file_from_fd(fd);
	if(!open_file)
	{
		return MAP_FAILED;
	}
	
	if(!(open_file->mode & O_RDONLY)) /* read access is required */
	{
		errno = EACCESS;
		return MAP_FAILED;
	}
	
	if(!((flags & MAP_PRIVATE) || (open_file->mode & O_WRONLY))) /* need write access unless mapped as a private copy */
	{
		errno = EACCESS;
		return MAP_FAILED;
	}
	
	if(flags & MAP_PRIVATE)
	/* writes are local;
	 * It's implimentation defined if writes to the underlying file affect reads so this is just a malloc and copy.
	 */
	{
		switch(open_file->type)
		{
			case __otter_fs_TYP_FILE:
				{
					struct __otter_fs_inode* inode = (struct __otter_fs_inode*)open_file->vnode;
					if(off > inode->size)
					{
						errno = EXIO;
						return MAP_FAILED;
					}
					
					if(off + len < off)
					{
						errno = EOVERFLOW;
						return MAP_FAILED;
					}
					
					if(off + len > inode->size)
					{
						errno = EXIO;
						return MAP_FAILED;
					}
					
					void *data = malloc(len);
					memcpy(data, (inode->data + off), len);
					return data;
				}
			case __otter_fs_TYP_ZERO: /* Common extension: mapping /dev/zero creates anonymous memory mapping (much like what malloc does) */
				{
					void* data = malloc(len);
					memset(data, 0, len);
					return data;
				}
			default: /* mapping file type not supported */
				errno = ENODEV;
				return MAP_FAILED;
		}
	}
	else
	/* Shared map, just return a pointer into the file.
	 * Note that more of the file may be mapped than just off+len.
	 * Normally this should only round up to the next page size,
	 * but here the whole file is already "mapped" so we just return a pointer into that memory
	 */
	{
		switch(open_file->type)
		{
			case __otter_fs_TYP_FILE:
				{
					struct __otter_fs_inode* inode = (struct __otter_fs_inode*)open_file->vnode;
					if(off > inode->size)
					{
						errno = EXIO;
						return MAP_FAILED;
					}
					
					if(off + len < off)
					{
						errno = EOVERFLOW;
						return MAP_FAILED;
					}
					
					if(off + len > inode->size)
					{
						errno = EXIO;
						return MAP_FAILED;
					}
					
					return (void*)(inode->data + off);
				}
			default: /* mapping file type not supported */
				errno = ENODEV;
				return MAP_FAILED;
		}
	}
}

/* Removes any mapping created by mmap in the specified memory range. This
 * works like free(), but uses len to determine how many consecutive pages to
 * free.  For memory mapped files in shared mode, the "mapping" is just a 
 * pointer into the file as stored in memory, which we do not want to free. For
 * private (local) mappings, the memory was allocated locally and should be
 * deallocated.  Since Otter does not support freeing part of a memory block,
 * the block must be entirely deallocated or not deallocated at all.
 */
int __otter_libc_munmap(void *addr, size_t len)
{
	if(len == 0)
	{
		errno = EINVAL;
		return(-1);
	}
	
	long pagesize = sysconf(_SC_PAGE_SIZE);
	if(!(len % pagesize == 0)) /* len must corrispond to an integer multiple of pages */
	{
		errno = EINVAL;
		return MAP_FAILED;
	}

	/* TODO: figure out a way to track when it is appropriate to free memory */
	return(0);
}
