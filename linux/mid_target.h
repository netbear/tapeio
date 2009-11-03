/***************************************************************************
*/
# ifndef __SCSI_TARGET__
# define __SCSI_TARGET__


# include <asm/io.h>
# include <asm/irq.h>
# include <asm/uaccess.h>  
# include <linux/delay.h>
# include <linux/dnotify.h>
# include <linux/file.h>
# include <linux/fs.h>
# include <linux/in.h>
# include <linux/init.h>
# include <linux/ioport.h>
# include <linux/kernel.h>
# include <linux/module.h>
# include <linux/pci.h>
# include <linux/sched.h>
# include <linux/smp_lock.h>
# include <linux/spinlock.h>
# include <linux/string.h>
# include <linux/types.h>
# include <linux/unistd.h>
# include <scsi/sg.h>

# include <../drivers/scsi/scsi.h>

# ifdef 	DEBUG_TARGET
# define 	debugInit(x...)	printk (x) //printk (##x)
# else
# define	debugInit(x...)
# endif

/* MODES OF OPERATION */

//# define MEMORYIO		/* for performance studies */
# define DISKIO		/* to a real disk */
//# define FILEIO		/* to a file on the system */
//# define GENERICIO		/* when the scsi_do_req is not working */

/* DEVICE type, default is DISK device */

//# define TAPE_DEVICE_ONLY		/* to a tape device */
//# define ROM_DEVICE		/* to a cdrom device */

#define  DEVICE_NAME_LENGTH	15 /* the string lenght od /dev/sda... */
#define PROTECT_SYSTEM_DISK

/* Magic Numbers */

# define	BLOCKSIZE	512
//# define	BLOCKSIZE	4096
# define	TWOBYTE		16
# define	BYTE		8
# define	FILESIZE	1900 * 1000 * 1000 /* file size in MB */
# define	IOFILE		1
# define	IOGENERIC	2
# define 	IO_SIGS		(sigmask(SIGKILL)|sigmask(SIGINT)|sigmask(SIGTERM)|sigmask(SIGIO))
# define	MAX_SENSE_DATA	16
# define 	SHUTDOWN_SIGS	(sigmask(SIGKILL)|sigmask(SIGINT)|sigmask(SIGTERM))
# define	TE_TRY		1
# define	TE_TIMEOUT	10*HZ
# define        MAX_HOST        255
#define NORMAL_RETRIES			5
#define IOCTL_NORMAL_TIMEOUT			(10 * HZ)
#define FORMAT_UNIT_TIMEOUT		(2 * 60 * 60 * HZ)
#define START_STOP_TIMEOUT		(60 * HZ)
#define READ_ELEMENT_STATUS_TIMEOUT	(5 * 60 * HZ)
#define READ_DEFECT_DATA_TIMEOUT	(60 * HZ )  /* ZIP-250 on parallel port takes as long! */


#define	MAX_LUNS	32

#define	MAX_DEVICES	32
#define   PAGES_PER_DEVICE   50
#define	BUFFER_PER_DEVICE  4
#define MAX_PAGES_PER_BUFFER 256


/* undefined scsi opcode */
# define	REPORT_LUNS	0xa0

/* the opcode our target manage */
# define  OPCODE_NUMBER		47

/* various possible states of commands - state in Target_Scsi_Cmnd */
# define	ST_NEW_CMND	1 /* command just arrived */
# define	ST_PROCESSING	2 /* sent off to process */
# define	ST_PENDING	3 /* waiting to get data */
# define	ST_TO_PROCESS	4 /* command ready to process */
# define	ST_DONE		5 /* response to command received */
# define	ST_DEQUEUE	6 /* front end done with command */
# define	ST_XFERRED	7 /* notified front end of buffers */
# define	ST_HANDED	8 /* command given to front end */
# define	ST_PROCESSED	9 /* SIGIO has been received */


/* values for abort code */
# define	CMND_OPEN	0 /* Normal state of command */
# define	CMND_ABORTED	1 /* ABORT received for this command */
# define	CMND_RELEASED	2 /* No response needed for this cmnd */

/* warning by netbear : redefined in scsi.h*/
#ifdef netbear
/* Values for management functions */
# define	ABORT_TASK		1	/*1000*/
# define	ABORT_TASK_SET	2	/*1001*/
# define	CLEAR_ACA		3	/*1002*/
# define	CLEAR_TASK_SET	4	/*1003*/
# define	LUN_RESET		5	/*1004*/
# define	TARGET_RESET	6	/*1005*/
#endif

/* command response lengths */
# define	READ_CAP_LEN	8
# define	ALLOC_LEN_6	4
# define	ALLOC_LEN_10	7
# define	LBA_POSN_10	2

/* variable definitions */

struct GTE;
struct SC;
struct SM;
struct STD;
struct STT;
struct stml_scatter;

/*
extern int 	target_name[MAX_DEVICES][5];
extern int 	device_num;
*/
extern int		device_num;

/* proc */
#ifdef CONFIG_PROC_FS
# define FALSE	0
# define TRUE	1 
# define MAX_RESULT_LENGTH   50
typedef struct proc_dir_entry PROC_DirectoryEntry;
typedef struct scsi_device Scsi_Device;

#endif

#ifdef DISKIO
typedef struct stml_scatter  /* holding area for scsi scatter gather info */
{
    unsigned short k_use_sg;    /* Count of kernel scatter-gather pieces */
    unsigned short sglist_len;  /* size of malloc'd scatter-gather list ++ */
    struct scatterlist   *scatterlist_buf;
    unsigned char * buf[PAGES_PER_DEVICE];
    unsigned int order[PAGES_PER_DEVICE];
    unsigned int page_size;
    unsigned int used;
    unsigned int cmd_id;
} stml_scatter_hold;    /* by netbear ?  bytes long on i386 */

typedef struct FD{
	/*
	*device name ,such as /dev/sda...
	*/
	char			target_device_name[DEVICE_NAME_LENGTH];
	/*
	* I think one device should had one thread?
	*/
	struct task_struct	*thread_id;
	/*
	*id,lun,etc
	*/
	unsigned int 	host_no,id,lun,channel;
	/*
	*
	*/
	unsigned int	stml_id;
	/*
	*the scsi device point
	*/
	Scsi_Device*	devp;
	/*
	*fot the list,next in queue
	*/
	struct FD	*next;
	/*
	*currnt cmd number
	*/
	unsigned int	cmd_number;
	/*
	*the mem hold
	*/
	stml_scatter_hold  reserve[BUFFER_PER_DEVICE];
	/*
	*lock for mem
	*/
	spinlock_t		get_mem;
	/*
	*just for debug and test:the command use the reserve
	*/
	unsigned int		cmd_use_res;
	/*
	*just for debug and test:current data under handle
	*/
	unsigned int		current_data;

	unsigned int		type;
	
}		stml_device;
#endif




typedef struct SM {
	/* next: pointer to the next message */
	struct SM	*next;
	/* prev: pointer to the previous message */
	struct SM	*prev;
	/* message: Task Management function received */
	int		message;
	/* device: device that received the Task Management function */
	struct STD	*device;
	/* value: value relevant to the function, if any */
	void		*value;
} Target_Scsi_Message;


typedef struct SC {
	/*
	 * This will take different values depending on what the present
	 * condition of the command is
	 */
	int		state;
	/* abort_code: is this command aborted, released, or open */
	int		abort_code;
	/* id: id used to refer to the command */
	unsigned int		id;
	/* dev_id: device id - front end id that received the command */
	__u64		dev_id;
	/* device: struct corresponding to device - may not be needed */
	struct STD	*device;
	/* dev_template: device template to be used for this command */
	struct STT	*dev_template;
	/* target_id: scsi id that received this command */
	__u64		target_id;
	 /* lun: which lun was supposed to get this command */
	__u64		lun;
	/* cmd: array for command until req is allocated */
	unsigned char	cmd[MAX_COMMAND_SIZE];
	/* len: length of the command received */
	int		len;
	/*
	 * queue of Scsi commands
	 */
	unsigned  long		timeout;
	/* next: pointer to the next command in the queue */
	struct SC	*next;
	/* req: this is the SCSI request for the Scsi Command */
    // warning by netbear :scsi_request is gone since linux 2.6.17
	// Scsi_Request	*req;
    // added by netbear
    int use_sg;
    int sglist_len;
    int buf_len;
    int result;
    int data_direction;
    unsigned char sense[SCSI_SENSE_BUFFERSIZE];
    struct scatterlist * sglist;
    stml_scatter_hold reserve;
	/* added by netbear */
	void * tmd_handle;
	int cmnd_read_buffer;
    uint64_t tmd_tag;

#ifdef DISKIO
	stml_device *stml_devp;
#endif
} Target_Scsi_Cmnd;


/* 
 * Scsi_Target_Template: defines what functions the added front end will
 * have to provide in order to work with the Target mid-level. The
 * comments should make each field clearer. MUST HAVEs define functions that
 * I expect to be there in order to work. OPTIONAL says you have a choice.
 * Also, pay attention to the fact that a command is BLOCKING or NON-BLOCKING
 * Although I do not know the effect of this requirement on the code - it
 * most certainly is required for efficient functioning.
 */
typedef struct STT
{
	/* private: */
	
	/*
	 * Do not bother to touch any one of these -- the only thing
	 * that should have access to these is the mid-level - If you
	 * change stuff here, I assume you know what you are doing :-)
	 */
	
	struct  STT *next;	/* next one in the list */
	int	device_usage;	/* how many are using this Template */

	/* public: */
    /*
     * The pointer to the /proc/scsi_target directory entry
     */
    struct proc_dir_entry *proc_dir;
 
    /* proc-fs info function.
     * Can be used to export driver statistics and other infos to the world
     * outside the kernel ie. userspace and it also provides an interface
     * to feed the driver with information. Check eata_dma_proc.c for reference
     */
    int (*proc_info)(char *, char **, off_t, int, int, int);

	/* 
	 * name of the template. Must be unique so as to help identify
	 * the template. I have restricted the name length to 16 chars
	 * which should be sufficient for most purposes. MUST HAVE
	 */
	const char name[TWOBYTE];
	/*
	 * detect function: This function should detect the devices that
	 * are present in the system. The function should return a value
	 * >= 0 to signify the number of front end devices within the
	 * system. A negative value should be returned whenever there is
	 * an error. MUST HAVE
	 */
	int (* detect) (struct STT*);
	/*
	 * release function: This function should free up the resources
	 * allocated to the device defined by the STD. The function should
	 * return 0 to indicate successful release or a negative value if
	 * there are some issues with the release. OPTIONAL
	 */
	int (* release)(struct STD*);
	/*
	 * xmit_response: This function is equivalent to the SCSI
	 * queuecommand. The front-end should transmit the response
	 * buffer and the status in the Scsi_Request struct. 
	 * The expectation is that this executing this command is 
	 * NON-BLOCKING.
	 * 
	 * After the response is actually transmitted, the front-end
	 * should call the front_end_done function in the mid-level
	 * which will allow the mid-level to free up the command
	 * Return 0 for success and < 0 for trouble
	 * MUST HAVE
	 */
	int (* xmit_response)(struct SC*);
	/*
	 * rdy_to_xfer: This function informs the driver that data
	 * buffer corresponding to the said command have now been
	 * allocated and it is okay to receive data for this command.
	 * This function is necessary because a SCSI target does not
	 * have any control over the commands it receives. Most lower
	 * level protocols have a corresponding function which informs
	 * the initiator that buffers have been allocated e.g., XFER_
	 * RDY in Fibre Channel. After the data is actually received
	 * the low-level driver needs to call rx_data provided by the
	 * mid-level in order to continue processing this command.
	 * Return 0 for success and < 0 for trouble
	 * This command is expected to be NON-BLOCKING.
	 * MUST HAVE.
	 */
	int (* rdy_to_xfer)(struct SC*);
	/*
	 * task_mgmt_fn_done: This function informs the driver that a
	 * received task management function has been completed. This
	 * function is necessary because low-level protocols have some
	 * means of informing the initiator about the completion of a
	 * Task Management function. This function being called will
	 * signify that a Task Management function is completed as far
	 * as the mid-level is concerned. Any information that must be
	 * stored about the command is the responsibility of the low-
	 * level driver. The field SC is relevant only for ABORT tasks
	 * No return value expected.
	 * This function is expected to be NON-BLOCKING
	 * MUST HAVE if the front-end supports ABORTs
	 */
	void (* task_mgmt_fn_done)(struct SM*);
	/*
	 * report_aen: This function is used for Asynchronous Event
	 * Notification. Since the Mid-Level does not have a mechanism
	 * to know about initiators logged in with low-level driver, it
	 * is the responsibility of the driver to notify any/all
	 * initiators about the Asynchronous Event reported.
	 * 0 for success, and < 0 for trouble
	 * This command is expected to be NON-BLOCKING
	 * MUST HAVE if low-level protocol supports AEN
	 */
	void (* report_aen)(int  /* MGMT FN */, u64 /* LUN */);
} Scsi_Target_Template;


/*
 * Scsi_Target_Device: This is an internal struct that is created to define
 * a device list in the struct GTE. Thus one STT could potentially
 * correspond to multiple devices. The only thing that the front end should
 * actually care about is the device id. This is defined by SAM as a 64 bit
 * entity. I should change this one pretty soon.
 */
typedef struct STD
{
	__u64		id;		/* device id */
	struct STD	*next; 		/* next one in the list */
	
	/* dev_specific: my idea behind keeping this field was that this
	 * should help the front end target driver store a pointer to a 
	 * struct that is a super-set of the Scsi_Target_Device where it
	 * stores system specific values. This should help the front-end
	 * locate the front-end easily instead of doing a search every
	 * time. (kind of like hostdata in Scsi_Host). Allocation and
	 * deallocation of memory for it is the responsibility of the 
	 * front-end. THIS IS THE ONLY FIELD THAT SHOULD BE MODIFIED BY
	 * THE FRONT-END DRIVER
	 */
	void		*dev_specific;
	
	Scsi_Target_Template *template;	/* ptr to available functions */
} Scsi_Target_Device;


/*
 * Target_Emulator: This is the "global" target struct. All information
 * within the system flows from this target struct. It also serves as a
 * repository for all that is global to the target emulator system
 */
typedef struct GTE
{
	/*
	 * command_id: This is a counter to decide what command_id gets 
	 * assigned to a command that gets queued. 0 is not a valid command.
	 * Also, command_ids can wrap around. I also assume that if command
	 * _ids wrap-around, then all commands with that particular command
	 * _id have already been dealt with. There is a miniscule (?) chance
	 * that this may not be the case - but I do not deal with it
	 */
	int			command_id;
	/*
	 * thread_sem: semaphore to control the synchronization of
	 * killing the thread
	 */
	struct semaphore	thread_sem;
	/*
	*add_del the devlist;
	*/
	spinlock_t		add_delete_device;
	/*
	*stml_device list
	*/
	stml_device	*devlist;
	/*
	*the number of the stml_device
	*/
	unsigned int	device_num;
	/*
	 * target_sem: semaphore to control the mid-level thread synchronizn
	 * i.e., when it sleeps and when it awakens
	 */
	struct semaphore	target_sem;
	/*
	 * thread_id: this task struct will store the pointer to the SCSI
	 * Mid-level thread - useful to kill the thread when necessary
	 */
	struct task_struct	*thread_id;
	/*
	 * st_device_list: pointer to the list of devices. I have not added
	 * any semaphores around this list simply because I do not expect
	 * multiple devices to register simultaneously (Am I correct ?)
	 */
	Scsi_Target_Device	*st_device_list;
	/*
	 * st_target_template: pointer to the target template. Each template
	 * in this list can have multiple devices
	 */
	Scsi_Target_Template	*st_target_template;
	/* this may change */
	/*
	 * add_delete: this spinlock must be acquired when a command is either
	 * added to the list or when it is removed from it. The spinlock is
	 * essential because received CDBs can get queued up when in the
	 * context of an interrupt handler so we have to spinlock
	 */
	spinlock_t		add_delete;
	/*
	 * queue_sem: this semaphore is acquired whenever one needs to change
	 * values within the queue. This cannot be used for adding and deleting
	 * commands
	 */
	struct semaphore	queue_sem;
	/*
	 * cmd_queue_start: pointer to the beginning of a queue
	 */
	Target_Scsi_Cmnd	*cmd_queue_start;
	/*
	 * cmd_queue_end: pointer to the end of the queue of commands
	 */
	Target_Scsi_Cmnd	*cmd_queue_end;
	/*
	 * msgq_start: pointer to the start of the message queue
	 */
	Target_Scsi_Message	*msgq_start;
	/*
	 * msgq_end: pointer to the end of the message queue
	 */
	Target_Scsi_Message	*msgq_end;
	/*
	 * msg_lock: spinlock for the message
	 */
	spinlock_t		msg_lock;
} Target_Emulator;


/* function prototypes */

/* these are entry points provided to the low-level driver */
int	register_target_template	(Scsi_Target_Template*);
int	deregister_target_template	(Scsi_Target_Template*);
int	scsi_target_done		(Target_Scsi_Cmnd*);
int	deregister_target_front_end	(Scsi_Target_Device*);
int	scsi_rx_data			(Target_Scsi_Cmnd*);
int	scsi_release			(Target_Scsi_Cmnd*);
stml_device *	get_devlist		(int);


Scsi_Target_Device* register_target_front_end 	(Scsi_Target_Template*);
Target_Scsi_Cmnd*	rx_cmnd		(Scsi_Target_Device*, __u64, 
					__u64, unsigned char*, int);
Target_Scsi_Message*	rx_task_mgmt_fn	(Scsi_Target_Device*,int,void*);


# endif
