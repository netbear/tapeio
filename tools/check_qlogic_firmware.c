#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <endian.h>

static void
firmware_frob(char *name, uint32_t *ptr, off_t size)
{
	uint32_t cp[(256 * 64) >> 2];
	int i;
	uint32_t la, wi, wl;

#if BYTE_ORDER == LITTLE_ENDIAN
	for (i = 0; i < size >> 2; i++) {
		la = ptr[i];
		ptr[i] = (la >> 24);
		ptr[i] |= (((la >> 16) & 0xff) << 8);
		ptr[i] |= (((la >>  8) & 0xff) << 16);
		ptr[i] |= (((la >>  0) & 0xff) << 24);
	}
#endif


	/*
	 * Keep loading until we run out of f/w.
	 */

	for (;;) {
		printf("%s: load 0x%x words of code at load address 0x%x\n", name, ptr[3], ptr[2]);
		wi = 0;
		la = ptr[2];
		wl = ptr[3];
		while (wi < ptr[3]) {
			uint32_t nw;

			nw = (256 * 64) >> 2;
			if (nw > wl) {
				nw = wl;
			}
			for (i = 0; i < nw; i++) {
				cp[i] = ptr[wi++];
				wl--;
			}
			la += nw;
		}
		if (ptr[1] == 0) {
			break;
		}
		ptr += ptr[3];
	}
}

int
main(int a, char **v)
{
	struct stat sbuf;
	void *ptr;
	int fd;

	while (--a) {
		fd = open(*++v, O_RDONLY);
		if (fd < 0) {
			perror(*v);
			continue;
		}
		if (fstat(fd, &sbuf) < 0) {
			perror("stat");
			close(fd);
			continue;
		}
		ptr = mmap(NULL, sbuf.st_size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
		if (ptr == NULL) {
			perror("mmap");
			close(fd);
			continue;	
		}
		firmware_frob(*v, ptr, sbuf.st_size);
		munmap(ptr, sbuf.st_size);
		close (fd);
	}
	return (0);
}
