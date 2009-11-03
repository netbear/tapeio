/* df revised begin */
#ifndef LINUX_VERSION_CODE
#include <linux/version.h>
#endif

#ifndef KERNEL_VERSION /* pre-2.1.90 didn't have it */
#define KERNEL_VERSION(vers,rel,seq) ( ((vers)<<16) | ((rel)<<8) | (seq) )
#endif
/* df revised end */

# include "mid_target.h"

//# define DEBUG_REGISTER
//# define DEBUG_DEREGISTER
//# define DEBUG_RX_CMND
//# define DEBUG_SCSI_DONE
//# define DEBUG_ALLOCN_LEN
//# define DEBUG_RX_DATA
//# define DEBUG_HANDLE_CMD
# define DEBUG_SCSI_THREAD
//# define DEBUG_TE_CMD


unsigned char  proc_result[MAX_RESULT_LENGTH];
int		MAX_COMMANDS_STML;
int		MAX_MESSAGES_STML;

/* FUNCTION DECLARATIONS */

int    scsi_target_process_thread(void*);

# if defined (FILEIO) || defined (GENERICIO)
static inline int    build_filp_table    (int);
static inline void    close_filp_table    (void);
# endif

//static inline int    get_inquiry_response     (Scsi_Request*, unsigned int);
//static inline int    get_read_capacity_response (Scsi_Request*);

static __u32   get_allocation_length(unsigned char*,unsigned int type);

static inline void abort_notify (struct SM*);
static inline void aen_notify (int, __u64);
static int get_space (Target_Scsi_Cmnd*, unsigned int);
static int time_out(Target_Scsi_Cmnd *);
static int free_tmd(Target_Scsi_Cmnd *);
static int hand_to_front_end(Target_Scsi_Cmnd*);
static int handle_cmd(struct SC*);

# ifdef GENERICIO
static int	fill_sg_hdr		(Target_Scsi_Cmnd*, int);
void		signal_process_thread	(void*);
# endif

# ifdef DISKIO
static void te_cmnd_processed (void * data, char * sense, int error, int datalen);
static inline int   fill_scsi_device(void);
static inline int   close_scsi_device(void);
# endif


#ifdef CONFIG_PROC_FS

/* proc fs  functions  and var*/
/*
static void CreateProcEntries(void);
static void DestroyProcEntries(void);
static int ProcReadStatus(char * buffer, char ** start, off_t offset,
			       int size, int * eof, void * data);
static int DeviceStatus(char * buffer, int * len, off_t * begin,
			       off_t offset, int size);
static int ProcReadUserCommand(char *Page, char **Start, off_t Offset,	int Count, int *EOF, void *Data);
static int ProcWriteUserCommand(struct file *File, const char *Buffer, unsigned long Count, void *Data);
static int ProcReadUserConfig(char *Page, char **Start, off_t Offset,	int Count, int *EOF, void *Data);
static int ProcWriteUserConfig(struct file *File, const char *Buffer, unsigned long Count, void *Data);
int ResetTargetMessage(void);
int ResetCommand(int lun);
int ResetLunMessage(int lun);
int CloseLun(int lun);
int StartLun(char * device);
int streql(char * str1,char * str2);*/

#ifdef DISKIO
int	CommandType[OPCODE_NUMBER];
unsigned char te_sg_buf[PAGE_SIZE];
typedef struct my_scsi_idlun {
/* why can't userland see this structure ??? */
    int dev_id;
    int host_unique_id;
} My_scsi_idlun;


//static int scan_dev_type(const char * leadin,My_scsi_idlun* my_idlun, int * host_no);
static void make_dev_name(char * fname, const char * leadin, int k, int do_numeric);
int	get_device_name(stml_device *  devp);
int	get_device_buffer(stml_device*  devp);
int	release_device_buffer(stml_device*);
/*
static int Debug(char * buffer, char ** start, off_t offset,
			       int size, int * eof, void * data);
static int DebugStatus(char * buffer, int * len, off_t * begin,
			       off_t offset, int size);
*/
#endif
/*
static PROC_DirectoryEntry *ProcDirectoryEntry;
*/
/* end proc--by zonk */

#endif

/* 
 * Scsi_Target_Device: Maximum number of "active" targets at any time
 * Scsi_Target_Template: Maximum number of templates available
 *
 * The naming convention for variables is with the prefix "st" following
 * Eric's comments in the SCSI Initiator mid-level.
 * 
 * The idea with the following variables is to have a generic linked
 * list of templates - there is one template for one "type" of device
 * whereas for each device there is a single entry in the device list
 */

Target_Emulator target_data;

/*
 * okay the following is to get something off the ground and working.
 * In the mode that the emulator is talking to a real disk drive, there
 * will be a need to map the id given to us by rx_cmnd to host, channel,
 * id as far as the SCSI mid-level is concerned. For the present moment
 * we map all of these to the same target. So all commands are directed
 * to different LUNs on a single device. I visualize this as being a
 * linked list of scsi disks attached to the system and whenever a
 * command is received, the rx_cmnd function does some id_lookup and map
 * the individual commands to one in the linked list. How this mapping
 * gets done is an upper level management function that I do not want to
 * deal with yet	ASHISH
 */
 /*
Scsi_Device	*scsi_target[MAX_DEVICES];
int		target_name[MAX_DEVICES][5];
char		target_device_name[MAX_DEVICES][DEVICE_NAME_LENGTH];// /dev/sda...
*/
int		device_num =1;

int		current_cmd_number = 0;
int		current_msg_number = 0;
spinlock_t	cmd_msg_number_lock;

unsigned long int  use_mem = 0;

/* the max space a command can apply */
int		MAX_SPACE = 8 * 4096*512;
/* Largest size (in bytes) a single scatter-gather list element can have.
   The value must be a power of 2 and <= (PAGE_SIZE * 32) [131072 bytes on
   i386]. The minimum value is PAGE_SIZE. If scatter-gather not supported
   by adapter then this value is the largest data block that can be
   read/written by a single scsi command. The user can find the value of
   PAGE_SIZE by calling getpagesize() defined in unistd.h . */

# ifdef GENERICIO
char		device[BYTE];
# endif

# ifdef FILEIO
char		device[BYTE*5];
# endif

/* 
 * scsi_target_init_module: Initializes the SCSI target module
 * FUNCTION: carry out all the initialization stuff that is needed
 * INPUT: none
 * OUTPUT: 0: everything is okay
 * 	      else trouble
 */
int scsi_target_init_module (void)
{
    int i;
    
    debugInit ("scsi_target_init_module enter ... \n");
    /* initialize the global target_data struct */
    /*
     * initialize the target semaphore as locked because you want
     * the thread to block after entering the first loop
     */
    init_MUTEX_LOCKED (&target_data.target_sem);
    init_MUTEX (&target_data.queue_sem);
    init_MUTEX_LOCKED (&target_data.thread_sem);

    target_data.add_delete          = SPIN_LOCK_UNLOCKED;
    target_data.msg_lock            = SPIN_LOCK_UNLOCKED;
    target_data.add_delete_device   = SPIN_LOCK_UNLOCKED;
    cmd_msg_number_lock             = SPIN_LOCK_UNLOCKED;
    target_data.st_device_list      = NULL;
    target_data.st_target_template  = NULL;
    target_data.cmd_queue_start     = NULL;
    target_data.cmd_queue_end       = NULL;
    target_data.msgq_start          = NULL;
    target_data.msgq_end            = NULL;
    target_data.command_id          = 0;
    target_data.thread_id           = NULL;
    target_data.device_num          = 0;

# ifdef DISKIO
    if( fill_scsi_device() == -1 ) {
        printk ("scsi_target_init_module: fill_scsi_device returned an error\n");
        return (-1);
    }
# endif
    
    /*
     * create a list of devices that are available. This is a very
     * simplistic mapping of these devices. Each new device will
     * represent a new LUN. A new mapping function will have to
     * solve this problem - LATER - Ashish
     */
# ifdef GENERICIO
    if (build_filp_table (IOGENERIC) < 0) {
        printk ("sc..init: build_filp_table returned an error\n");
        return (-1);
    }
# endif
    
# ifdef FILEIO
    if (build_filp_table (IOFILE) < 0) {
        printk ("sc..init: build_filp_table returned an error\n");
        return (-1);
    }
# endif
 
#ifdef CONFIG_PROC_FS
    //CreateProcEntries();
    MAX_COMMANDS_STML = 10240;
    MAX_MESSAGES_STML  = 10240;
    sprintf(proc_result,"Init OK\n");
    #ifdef DISKIO
    for(i=0;i<OPCODE_NUMBER;i++)
        CommandType[i] = 0;
    #endif
#endif

    


    /* spawn a thread */
    kernel_thread((int (*)(void *)) scsi_target_process_thread, NULL, 0);
    debugInit ("exit\n");
    return (0);
}


/* 
 * scsi_target_cleanup_module: Removal of the module from the kernel
 * FUNCTION: carry out the cleanup stuff
 * INPUT: None allowed
 * OUTPUT: None allowed
 */
void scsi_target_cleanup_module (void)
{
    int r;
# ifdef GENERICIO
    
    /* close all the devices that you have opened */
    close_filp_table ();

    if (target_data.signal_id != NULL)
        send_sig (SIGKILL, target_data.signal_id, 1);
    down_interruptible (&target_data.signal_sem);
# endif

# ifdef FILEIO
    close_filp_table();
# endif

#ifdef DISKIO
    close_scsi_device();
#endif

#ifdef CONFIG_PROC_FS
        //DestroyProcEntries();
#endif

    /* kill the thread */
    if (target_data.thread_id != NULL)
        send_sig (SIGKILL, target_data.thread_id, 1);
    r = down_interruptible (&target_data.thread_sem);
    if (r)
        printk("in cleanup module : interrupted %d\n", r);
    printk ("Exiting scsi module\n");
}


module_init (scsi_target_init_module);
module_exit (scsi_target_cleanup_module);

/*
 * register_target_template: to register a template with the midlevel
 * FUNCTION: to add the midlevel template to the linked list
 * INPUT: pointer to the template
 * OUTPUT: 0 for successful registration
 * 	   < 0 for trouble
 */
int register_target_template (Scsi_Target_Template *the_template)
{
    int check;
    Scsi_Target_Template *st_current; 

    if (!the_template) { /* huh */
        printk ("register_target_template: Cowardly refusal to register a NULL template\n");
        return (-1);
    }
    
    the_template->device_usage = 0;

    /* check if the template is already in the list */
    for (st_current = target_data.st_target_template; st_current != NULL; st_current = st_current->next)
    {
        /* loop */
# ifdef DEBUG_REGISTER
        printk ("Looping template\n");
# endif
        if (!strcmp (target_data.st_target_template->name, the_template->name))
            break;
    }
    
    if (st_current != NULL) { /* template is old */
        printk ("register_target_template: Template already present\n");
        return (-1);
    }


    the_template->next = target_data.st_target_template;
    target_data.st_target_template = st_current = the_template;

# ifdef DEBUG_REGISTER
    printk ("module count increased\n");
# endif
    //MOD_INC_USE_COUNT;

    /* check if the device has a detect function */
    if (st_current->detect) {
        /* detect the device */
        check = st_current->detect(the_template);
        if (check < 0) { /* error */
            printk ("Hosts detected: %d .. removing template\n", check);
            deregister_target_template (the_template);
            return (check);
        }
        else {
            printk ("Hosts detected: %d ... incrementing device usage\n", check);
        }
    }

    else {
        printk ("register_target_template: template does not have a detect function\n");
        deregister_target_template (the_template);
        return (-1);
    }

    return (0);
}
EXPORT_SYMBOL(register_target_template);


/*
 * deregister_target_template:
 * FUNCTION: removal of the target template from the list
 * INPUT: pointer to STT
 * OUTPUT: 0 for successful deregistration
 *        else < 0 for trouble
 */
int deregister_target_template (Scsi_Target_Template *the_template)
{
    Scsi_Target_Template    *st_current, *st_prev = NULL;
    Scsi_Target_Device    *st_dev_curr;
    
    st_dev_curr = target_data.st_device_list;

    if (!the_template) { /* huh */
        printk ("deregister_target_template: Cannot remove a NULL template\n");
        return (-1);
    }

    /* remove devices that are using this template */
    while (st_dev_curr && st_dev_curr->template && (st_dev_curr->template->device_usage > 0)) {
        st_dev_curr    = target_data.st_device_list;
        while (st_dev_curr) {
            if (!strcmp(st_dev_curr->template->name, the_template->name))
            {
# ifdef DEBUG_DEREGISTER
                printk ("dereg...tmpt: Match found for %s%d\n", st_dev_curr->template->name, (int)st_dev_curr->id);
# endif
                deregister_target_front_end (st_dev_curr);
                st_dev_curr = target_data.st_device_list; // Edward
                break;
            }
            else {
                st_dev_curr = st_dev_curr->next;
                if (st_dev_curr) {
                    printk ("dereg..tmpt: Error ... no device found with the required template %s\n", the_template->name);
                    return (-1);
                }
            }
        }
    }


    /* remove template from device list */
    for (st_current = target_data.st_target_template; st_current != NULL; st_current = st_current->next)
    {
# ifdef DEBUG_DEREGISTER
        printk ("Looping dereg template\n");
# endif
        if (!strcmp (st_current->name, the_template->name)) {
# ifdef DEBUG_DEREGISTER
            /* match found */
            printk ("deregister_template: template match found\n");
# endif
            /* check if there are any devices using this template */
            if (!st_current->device_usage) {
                /* this template can be removed */
                if (!st_prev) /* First element */
                    target_data.st_target_template = target_data.st_target_template->next;
                else /* middle of the road */
                    st_prev->next = st_current->next;
                break;
            }
            else {/* A device still uses the template */
				printk ("scsi_target: Non-zero device usage ouch !!\n");
				return (-1); /* should never get here */
			}
		}
		else st_prev = st_current;
	}
	
# ifdef DEBUG_DEREGISTER
	printk ("Decreasing module count\n");
# endif
	//MOD_DEC_USE_COUNT;

	return (0);
}
EXPORT_SYMBOL(deregister_target_template);


/*
 * register_target_front_end:
 * FUNCTION: 	to register the individual device with the mid-level
 * 		to allocate the device an device id etc
 * 		start a thread that will be responsible for the target
 * INPUT:	pointer to a struct STD
 * OUTPUT:	Scsi_Target_Device - if everything is okay
 * 		else NULL if there is trouble
 */
Scsi_Target_Device *register_target_front_end (Scsi_Target_Template *tmpt)
{
    Scsi_Target_Device *the_device;
    
    the_device = (Scsi_Target_Device *)kmalloc (sizeof (Scsi_Target_Device), GFP_KERNEL);
    if (!the_device) {
        printk ("register_target_front_end: Could not allocate space for the device\n");
        return (NULL);
    }
    
    if (!tmpt) {
        printk ("register_target_front_end: Cannot register NULL device template !!!\n");
        return (NULL);
    }
    
    /* fill up the struct */
    the_device->template = tmpt;

    the_device->next = target_data.st_device_list;
    
    if (the_device->next)
        the_device->id = the_device->next->id + 1;
    else
        the_device->id = 0; /* first device */

    target_data.st_device_list    = the_device;

# ifdef DEBUG_DEREGISTER
    printk ("reg..end: device %s%d added\n", tmpt->name, (int)the_device->id);
# endif
 

    tmpt->device_usage++;
    return (the_device);
}
EXPORT_SYMBOL(register_target_front_end);

/*
 * deregister_target_front_end:
 * FUNCTION:    to allow removal of the individual device from the
 *         midlevel
 *         free up the device id number for future use
 *         close the thread responsible for the target after making
 *         sure that all existing commands have been responded to
 *         we need to do serious error checking since this can be
 *         called by different front-ends
 *         CANNOT BE CALLED FROM INTERRUPT CONTEXT
 * INPUT:    pointer to a struct STD to be removed
 * OUTPUT:    int - 0 if everything is okay
 *         else < 0 if there is trouble
 */
int deregister_target_front_end (Scsi_Target_Device *the_device)
{
    Scsi_Target_Device    *curr, *previous = NULL;
    Target_Scsi_Cmnd    *cmnd;
    
    if (!the_device) {
        printk ("dereg...end: cannot remove NULL devices corresponding to a NULL template\n");
        return (-1);
    }
    
    if (!the_device->template->device_usage) {
        printk ("dereg...end: 0 device usage and a device to deregister ... a contradiction me thinks\n");
    }
    

    /* 
     * go through the device list till we get to this device and
     * then remove it from the list
     */
    for (curr = target_data.st_device_list; curr != NULL; curr = curr->next)
    {
        if (curr == the_device) {
# ifdef DEBUG_DEREGISTER
            printk ("dereg..end: We have a match\n");
# endif
            break;
        }
        else previous = curr;
    }

    if  (!curr) { /* No match found */
        printk ("dereg..end: No match found\n");
        return (-1);
    }

    /* remove it from the list */
    if (previous) /* not the first device */
        previous->next = curr->next;

    else    target_data.st_device_list = curr->next;

    /* release the device */
    if (curr->template->release) {
        if (curr->template->release (curr)) {
            printk ("dereg...end: release of device failed\n");
            return (-1);
        }
    }

    /* drop all commands corresponding to this device */
    down (&target_data.queue_sem);
    for (cmnd =  target_data.cmd_queue_start; cmnd != NULL; cmnd = cmnd->next)
    {
        if (cmnd->dev_id == curr->id)
            cmnd->state = ST_DEQUEUE;
    }
    up (&target_data.queue_sem);

    /* wake up the mid-level thread so it can dequeue stuff */
    up (&target_data.target_sem);

    /* reduce device usage */
    curr->template->device_usage--;

    /* freeing things */
    kfree (curr);
    curr    = NULL;
    
    return (0);
}
EXPORT_SYMBOL(deregister_target_front_end);


/*
 * scsi_target_process_thread: this is the mid-level target thread that
 * is responsible for processing commands.
 */
int scsi_target_process_thread (void *param)
{
# ifdef GENERICIO
    mm_segment_t old_fs;
    struct file *dev_file;
# endif
    Target_Scsi_Cmnd *cmd_curr, *cmd_copy, *cmd_prev = NULL;
    Target_Scsi_Message    *msg;
    unsigned long flags;
    int r;
# ifdef DISKIO
    Scsi_Device *this_device = NULL;
# endif
    
    lock_kernel(); 
    daemonize("mid_target_thread");
    siginitsetinv(&current->blocked, SHUTDOWN_SIGS);
    target_data.thread_id = current;
    unlock_kernel();

# ifdef DEBUG_SCSI_THREAD
    printk ("STARTING SCSI_TARGET_THREAD\n");
# endif
    
    while (1) {
        
        r = down_interruptible (&target_data.target_sem);
        if (r)
            printk("in mid target processed thread : interrupted %d\n", r);

        /* is signal received */
        if (signal_pending (current)) {
# ifdef DEBUG_SCSI_THREAD
            printk ("scsi_target_process_thread: Signal got\n");
# endif
            goto scsi_thread_out;
        }
        
        /* is message received */
        while (target_data.msgq_start) {
            /* house keeping */
            spin_lock_irqsave (&target_data.msg_lock, flags);
            msg = target_data.msgq_start;
            target_data.msgq_start = msg->next;
            if (msg->next)
                msg->next->prev = NULL;
            if (target_data.msgq_end == msg)
                target_data.msgq_end = NULL;
            spin_unlock_irqrestore (&target_data.msg_lock, flags);
            printk("mid_target : receiving message %d\n", msg->message); 

#ifdef CONFIG_PROC_FS
            spin_lock_irqsave (&cmd_msg_number_lock, flags);
            current_msg_number--;
            spin_unlock_irqrestore (&cmd_msg_number_lock, flags);
#endif

            /* execute function */
            switch (msg->message) {
                case ABORT_TASK: 
                    {
                        Target_Scsi_Cmnd *cmnd;
                        cmnd = (Target_Scsi_Cmnd*) msg->value;
                        for (cmd_curr = target_data.cmd_queue_start; cmd_curr != NULL; cmd_curr = cmd_curr->next)
                        {
                            if ((cmd_curr->id == cmnd->id) && (cmd_curr->lun == cmnd->lun))
                                break;
                        }
                        
                        if (cmd_curr) {
                            cmd_curr->abort_code = CMND_ABORTED;
                            if (cmd_curr->state != ST_PROCESSING)
                                cmd_curr->state = ST_DEQUEUE;
                            abort_notify (msg);
                        }
                        else
                            printk ("scsi..thread: Could not find command in list\n");
                        break;
                    }
                
                /* by netbear : Not defined in scsi.h
                 * case LUN_RESET:
                    {
                        // BAD BAD REALLY BAD
                        __u64 lun = *((__u64*) msg->value);
                        for (cmd_curr = target_data.cmd_queue_start; cmd_curr != NULL; cmd_curr = cmd_curr->next)
                        {
                            if(cmd_curr->target_id == lun)//if (cmd_curr->lun == lun)// is this true? zonk
                                scsi_release (cmd_curr);
                        }
                        
                        aen_notify (msg->message, lun);
                        break;
                    }*/

                case TARGET_RESET:
                    {
                        for (cmd_curr = target_data.cmd_queue_start; cmd_curr != NULL; cmd_curr = cmd_curr->next)
                            scsi_release (cmd_curr);
                        //warning by netbear: break here??
                        break;
                        aen_notify (msg->message, 0);
                    }
                
                default:
                    {
                        printk ("scsi..thread: Should never get here\n");
                        break;
                    }
            }
            
            kfree (msg);
            msg = NULL;
        }
        
# ifdef DEBUG_SCSI_THREAD
        printk ("sc..thread: Searching command queue\n");
# endif
        cmd_curr = target_data.cmd_queue_start;
        cmd_prev = NULL;
        while (cmd_curr) {
# ifdef DEBUG_SCSI_THREAD
            printk ("sc..thread: command %x id %d status %d, tag %llx\n", cmd_curr->cmd[0], cmd_curr->id, cmd_curr->state, cmd_curr->tmd_tag);
# endif
            /* is command received */
            if (cmd_curr->state == ST_NEW_CMND) {
# ifdef DEBUG_SCSI_THREAD
                printk ("sc..thread: New command %p id: %d\n", cmd_curr, cmd_curr->id);
# endif
                
                if(cmd_curr->stml_devp)
                    this_device = cmd_curr->stml_devp->devp;
                else
                {
                    printk("sc..thred:device removed for the command %d\n",cmd_curr->id);
                    cmd_curr->state = ST_DEQUEUE;
                }
                
                if( !this_device )
                {    
                    printk( "sc..thread: Can't found the device\n" );
                    goto scsi_thread_out;
                }

                /*if (!cmd_curr->req) {
                    printk ("sc..thread: req: kmalloc failed\n");
                    goto scsi_thread_out;
                }
                
                memcpy (cmd_curr->req->sr_cmnd, cmd_curr->cmd, cmd_curr->len);*/

                if (handle_cmd(cmd_curr)) {
                    /* is bailing out a good idea */
                    goto scsi_thread_out;
                }
            }

            /* is a command pending */
            if (cmd_curr->state == ST_PENDING) {
# ifdef DEBUG_SCSI_THREAD
                printk ("sc..thread: Command %p id: %d pending\n", cmd_curr, cmd_curr->id);
# endif
                /* call the rdy_to_xfer function */
                if (hand_to_front_end (cmd_curr)) {
                    printk ("sc..thread: hand_to_front_end: returned an error for command %d\n", cmd_curr->id);
                    goto scsi_thread_out;
                }
            }
            
            /* is data received */
            if (cmd_curr->state == ST_TO_PROCESS) {
                /*
                 * we have received the data - does this
                 * go off to handle_cmd again ?
                 */
# ifdef DEBUG_SCSI_THREAD
                printk ("sc..thread: Command %p id: %d with data received\n", cmd_curr, cmd_curr->id);
# endif
                if (handle_cmd(cmd_curr)) {
                    printk ("sc..thread: handle_cmd: err for command %d\n", cmd_curr->id);
                    /* is bailing out a good idea */
                    goto scsi_thread_out;
                }
            }


            /* is command done */
            if (cmd_curr->state == ST_DONE) {
                /* 
                 * hand it off to the front end driver
                 * to transmit
                 */
# ifdef DEBUG_SCSI_THREAD
                printk ("sc..thread: Command %p id: %d done\n", cmd_curr, cmd_curr->id);
# endif
                if (hand_to_front_end (cmd_curr)) {
                    printk ("sc..thread: hand_to_front_end: returned an error for command %d\n", cmd_curr->id);
                    goto scsi_thread_out;
                }
            }

            /* can command be dequeued */
            if (cmd_curr->state == ST_DEQUEUE) {
                /*
                 * dequeue the command and free it
                 */
# ifdef DEBUG_SCSI_THREAD
                printk ("sc..thread: Command %p id: %d - to dequeue\n", cmd_curr, cmd_curr->id);
                if (cmd_curr->next)
                    printk ("sc..thread: Command %p id: %d - in queue\n", cmd_curr->next, cmd_curr->next->id);
# endif
                /* so that we can repoint cmd_curr */
                cmd_copy = cmd_curr; 
 
                /* dequeue the command */
                spin_lock_irqsave (&target_data.add_delete, flags);
                
                if (cmd_prev) {
                    cmd_prev->next    = cmd_curr->next;
                    if (cmd_curr == target_data.cmd_queue_end) /* Last element */
                        target_data.cmd_queue_end = cmd_prev;
                }

                else { /* first element */
                    if (target_data.cmd_queue_end == cmd_curr) /* only element */
                    {
                        target_data.cmd_queue_end = NULL;
                        target_data.cmd_queue_start = NULL;
                    }

                    else target_data.cmd_queue_start = cmd_curr->next;
                }

                /* reset pointers */
                cmd_curr = cmd_curr->next;
                spin_unlock_irqrestore (&target_data.add_delete, flags);

                #ifdef CONFIG_PROC_FS
                spin_lock_irqsave (&cmd_msg_number_lock, flags);
                current_cmd_number--;
                spin_unlock_irqrestore (&cmd_msg_number_lock, flags);
                #endif

//                printk("sc..thread:delete the cmd from list.id:%d\n",cmd_copy->id);

                free_tmd(cmd_copy);
                
                cmd_copy = NULL;
            }

            /* if we get through to here then move on */
            else if (cmd_curr) {
                /* proceed to the next command */
# ifdef DEBUG_SCSI_THREAD
                printk ("sc..thread: Moving to the command after %p, id: %d\n", cmd_curr, cmd_curr->id);
# endif
                if(jiffies>cmd_curr->timeout)
                {
//                    printk("time out!!!\n");
                    time_out(cmd_curr);
                }
                cmd_prev = cmd_curr;
                cmd_curr = cmd_curr->next;
            }
        }
# ifdef DEBUG_SCSI_THREAD
        printk ("sc..thread: going back to sleep again\n");
# endif
    }
scsi_thread_out:
    printk ("before up\n");
    up (&target_data.thread_sem);
    return 0;
}

//add by cherish 3.24 2003
stml_device *	get_devlist(int initdev)
{
	if(initdev)
		if(fill_scsi_device() != 0)
		{
			printk("get_devlist error!\n");
			return NULL;
		}
	return target_data.devlist;
}
EXPORT_SYMBOL(get_devlist);
//end

/*
 * rx_cmnd: this function is the basic function called by any front end
 * when it receives a SCSI command. The rx_cmnd function then fills up
 * a Target_Scsi_Cmnd struct, adds to the queue list, awakens the mid-
 * level thread and then returns the struct to the front end driver
 * INPUT:	device, target_id, lun, SCSI CDB as an unsigned char
 * 		    length of the command (or size of scsi_cdb array if 
 * 		    unavailable
 * OUTPUT:	Target_Scsi_Cmnd struct or NULL if things go wrong
 */
Target_Scsi_Cmnd *rx_cmnd (Scsi_Target_Device *device, __u64 target_id, 
               __u64 lun, unsigned char *scsi_cdb, int len)
{
    Target_Scsi_Cmnd *command;
    unsigned long flags;

#ifdef DISKIO
    stml_device *temp;
#endif

    if (!target_data.thread_id) {
        printk ("rx_cmnd: No Mid-level running !!!!\n");
        return NULL;
    }
    
    if (!device) {
        printk ("rx_cmnd: No device given !!!!\n");
        return NULL;
    }


    #ifdef CONFIG_PROC_FS//this should be changed  --zonk
    /*
    if(current_cmd_number>=MAX_COMMANDS_STML)
    {
        printk("rx_cmd: MAX Commands exist!!!\n");
        return NULL;
    }
    */
    #endif

    command = (Target_Scsi_Cmnd*) kmalloc (sizeof (Target_Scsi_Cmnd), GFP_KERNEL|GFP_ATOMIC);
    if (command == NULL) {
        printk ("rx_cmnd: kmalloc: command ... No space\n");
        /* sendsig (SIGKILL, target_data.thread_id, 0); */
        return NULL;
    }
    
# ifdef DEBUG_RX_CMND
    printk ("rx_cmnd: filling up command struct %p\n", command);
# endif
    
    /* fill in Target_Scsi_Cmnd */
    if (scsi_cdb != NULL) {
        memcpy (command->cmd, scsi_cdb, MAX_COMMAND_SIZE);
        command->state         = ST_NEW_CMND;
        command->abort_code    = CMND_OPEN;
        command->device        = device;
        command->dev_id        = device->id;
        
        /* cdeng change target_id if lun doesn't match in the future */
        command->target_id     = target_id;
        command->lun           = lun;
        command->next          = NULL;
        if ((len < MAX_COMMAND_SIZE) && (len > 0))
            command->len       = len;
        else {
//            printk ("setting cmd len to %d instead of %d\n", MAX_COMMAND_SIZE, len);
            command->len       = MAX_COMMAND_SIZE;
        }
        command->cmnd_read_buffer = 0;
        command->timeout       = jiffies + 480*100*HZ; 

# if defined (FILEIO) || defined (GENERICIO)
        /* fill in the device descriptor */
        command->fd = NULL;
        for (temp = target_data.fdlist; temp != NULL; temp = temp->next)
        {
# ifdef DEBUG_RX_CMND
            printk ("rx_cmnd: looking for id: %d, lun: %d: in loop for id: %d, lun: %d\n", (int) command->target_id, (int) command->lun, (int) temp->target_id, (int) temp->lun);
# endif

            if ((command->target_id == temp->target_id) && 
                (command->lun == temp->lun))
            {
# ifdef DEBUG_RX_CMND
                printk ("rx_cmnd: Match found\n");
# endif
                command->fd = temp;
                break;
            }
        }

        if (command->fd == NULL) { /* YEAH, BABY !! */
            printk ("rx_cmnd: Could not find a match for the given command\n");
            kfree (command); //Edward
            return NULL;
        }
# endif

#ifdef DISKIO
        command->stml_devp = NULL;
        for(temp = target_data.devlist;temp != NULL;temp = temp->next)
        {
            if((command->target_id == temp->stml_id)&&(command->lun == temp->lun))// by netbear : &&(temp->devp->online == TRUE))
            {
                command->stml_devp = temp;
                temp->cmd_number++;
            }
        }
        if(command->stml_devp==NULL)
        {
            printk("rx_cmnd:Could not find a match for the given command.id:%d lun:%d\n",(int)command->id,(int)command->lun);
            kfree(command);
            return NULL;
        }
#endif

        /*
         * this could potentially be gross
         */
        spin_lock_irqsave(&target_data.add_delete, flags);
# ifdef DEBUG_RX_CMND
        printk ("rx_cmnd: spinlock for %p\n", command);
# endif
        command->id = ++target_data.command_id;
        /* check this to make sure you dont have a command with this id ????? IGNORE FOR NOW */
        if (!command->id) { /* FOR WRAP AROUNDS */
            command->id = ++target_data.command_id;
        }

        if ((!target_data.cmd_queue_start) && (!target_data.cmd_queue_end)) {
            target_data.cmd_queue_start= target_data.cmd_queue_end = command;
        }
        else  {
            target_data.cmd_queue_end->next = command;
            target_data.cmd_queue_end    = command;
        }

# ifdef DEBUG_RX_CMND
        printk ("rx_cmnd: spinlock for %p with id %d\n", command, command->id);
# endif
        spin_unlock_irqrestore (&target_data.add_delete, flags);

        #ifdef CONFIG_PROC_FS
        spin_lock_irqsave (&cmd_msg_number_lock, flags);
        current_cmd_number++;
        spin_unlock_irqrestore (&cmd_msg_number_lock, flags);
        #endif
        
        // printk("rx_cmnd: add the cmd into list.id:%d\n",command->id);
    }
/*
    printk("for celts %d:%2x %2x %2x %2x %2x %2x %2x %2x %2x %2x\n",
    command->id,
    command->cmd[0],
    command->cmd[1],
    command->cmd[2],
    command->cmd[3],
    command->cmd[4],
    command->cmd[5],
    command->cmd[6],
    command->cmd[7],
    command->cmd[8],
    command->cmd[9]);
*/
    /* wake up thread */
    up (&target_data.target_sem);
    return (command);
}
EXPORT_SYMBOL(rx_cmnd);

/*
 * scsi_rx_data: This function is called by the lower-level driver to 
 * let the mid-level that data corresponding to a command has been 
 * received. This function can be called from within an interrupt 
 * handler (??). All this function does currently is to change the 
 * state of a command and then wake the mid-level thread to deal with 
 * this command.
 * INPUT: scsi command for which data has been received
 * OUTPUT: 0 if okay else < 0
 */
int scsi_rx_data (Target_Scsi_Cmnd *the_command)
{
		Target_Scsi_Cmnd	*curr = target_data.cmd_queue_start;

		if (!the_command) {
				printk ("rx_data: cannot deal with NULL command\n");
				return (-1);
		}

		/* check to see if the command exists in the queue */
		while (curr) {
				if (curr->id == the_command->id) {
# ifdef DEBUG_RX_DATA
						printk ("scsi_rx_data: Match found\n");
# endif
						curr->state = ST_TO_PROCESS;
						break;
				}
				curr = curr->next;
		}

		if (!curr) { /* this command may have been aborted */
				printk ("rx_data: could not find a match for the command\n");
				printk ("rx_data: This may not be serious ... the command may have been aborted\n");
				return (-1);
		}

		/* wake up the mid-level thread */
		up (&target_data.target_sem);
				
		return (0);
}

EXPORT_SYMBOL(scsi_rx_data);

/*
 * scsi_target_done: This is the function called by the low-level driver
 * to signify that it has completed the execution of a given scsi cmnd
 * This function needs to remove the resources that have been allocated
 * to the given command, dequeue the command etc. This function will be
 * called from within the context of an interrupt handler. So, it may
 * be best to leave it up to the mid-level thread to actually deal with
 * these functions. Right now, I am setting it up so that the status of
 * the command is changed and the mid-level is awakened.
 * INPUT: scsi command to be dequeued
 * OUTPUT: 0 if everything is okay
 * 	   < 0 if there is trouble
 */
int scsi_target_done (Target_Scsi_Cmnd *the_command)
{
	Target_Scsi_Cmnd	*curr = target_data.cmd_queue_start;
	
	if (!the_command) {
		printk ("scsi_target_done: cannot dequeue NULL command\n");
		return (-1);
	}

	/* check to see if the command exists in the queue */
	while (curr) {
		if (curr->id == the_command->id) {
# ifdef DEBUG_SCSI_DONE
			printk ("scsi_target_done:Match found,cmdid=%d\n",the_command->id);
# endif
			curr->state = ST_DEQUEUE;
			break;
		}
		curr	= curr->next;
	}

	if (!curr) { /* should never happen */
		printk ("scsi_target_done: could not find a match for the command\n");
		/*
		 * This error may not be so critical. We should be able
		 * to do away with this command without affecting things
		 * For now kill everything
		 */
		return (-1);
	}

	/* in the clear - awaken the scsi_thread to do the needful */
	up (&target_data.target_sem);

	return (0);
}
EXPORT_SYMBOL(scsi_target_done);


/*
 * scsi_release: This function is called by a low-level driver when it
 * determines that it does not responses to certain commands. Situations
 * like this happen when for instance a LIP is received in a Fibre
 * Channel Loop or when a Logout is received in iSCSI before command
 * execution is completed. The low-level driver may no longer care about
 * receiving responses for those commands. This function can be called
 * from within interrupt context
 * INPUT: command to release
 * OUTPUT: 0 if success, < 0 if there is trouble
 */
int scsi_release (Target_Scsi_Cmnd *cmnd)
{
	Target_Scsi_Cmnd	*curr = target_data.cmd_queue_start;

	if (!cmnd) {
		printk ("scsi_release: Cannot release a NULL command!!!\n");
		return (-1);
	}

	/* check to see if the command exists in our queue */
	while (curr) {
		if (curr->id == cmnd->id) {
			curr->abort_code = CMND_RELEASED;
			/* 
			 * if a command is processing, it is not nice to
			 * dequeue a command, as we will get a response
			 * anyways. This may be an inherent race condn
			 * We catch what we can and move on. A second 
			 * check performed when hand_to_front_end is
			 * called should catch the remaining commands
			 * - Ashish
			 */
			if (curr->state != ST_PROCESSING)
				curr->state = ST_DEQUEUE;
			break;
		}
		curr = curr->next;
	}

	if (!curr) {
		printk ("scsi_release: A match for the command was not found\n");
		return (-1);
	}

	up (&target_data.target_sem);
	return (0);
}


/*
 * rx_task_mgmt_fn: This function is called by a low-level driver to
 * indicate to the Mid-level that it has received a management function.
 * This function will decide the action to be taken in response to this
 * management function. This function will in turn create a message list
 * in the mid-level. CAN BE CALLED FROM INTERRUPT CONTEXT
 * INPUT: device, function, command - if relevant
 * OUTPUT: message or NULL if there is trouble
 * Definition of value:
 * 1. ABORT
 * 	value = Target_Scsi_Cmnd
 * 2. ABORT TASK SET
 * 	This function cannot be directly performed since the Mid-Level
 * 	has no knowledge of what Initiators are logged in to the front-
 * 	end. This functionality can be achieved by issuing ABORTS to 
 * 	various commands identified within the Task Set by the front
 * 	-end. value = N/A
 * 3. CLEAR ACA
 * 	I dont understand this one
 * 4. CLEAR TASK SET
 * 	Implement similar to ABORT Task Set - I know that all semantics 
 * 	for this cannot be implemented (Any ideas ?)
 * 5. LUN RESET
 * 	set value = pointer to LUN
 * 6. TARGET RESET
 * 	set value = NULL
 */
struct SM* rx_task_mgmt_fn (struct STD *dev, int fn, void *value)
{
	unsigned long flags;
	Target_Scsi_Message	*msg;

	msg	= (Target_Scsi_Message*) kmalloc (sizeof (Target_Scsi_Message), GFP_KERNEL | GFP_ATOMIC);
	if (!msg) {
		printk ("rx_task_mgmt_fn: could not allocate space for the scsi message\n");
		return NULL;
	}

	if ((fn < ABORT_TASK) && (fn > TARGET_RESET)) {
		printk ("rx_task_mgmt_fn: Invalid value for Task Management function\n");
		return NULL;
	}

	if ((fn == ABORT_TASK_SET) || (fn == CLEAR_ACA) || (fn == CLEAR_TASK_SET))
	{
		printk ("rx_task_mgmt_fn: This task management function is not implemented. See the comments above the rx_task_mgmt_fn definition\n");
		return NULL;
	}

	else if ((fn == ABORT) && (value == NULL)) {
		printk ("rx_task_mgmt_fn: Cannot abort a NULL command\n");
		return NULL;
	}

	else if (fn == TARGET_RESET) { // by netbear : not defined in scsi.h if ((fn == LUN_RESET) || (fn == TARGET_RESET)) {
		if (value != NULL) { /* not a critical error */
			printk ("rx_task_mgmt_fn: setting a value here is irrelevant. Ignoring this value and executing the function anyways\n");
		}
	}

	#ifdef CONFIG_PROC_FS
	/*
	if(current_msg_number>=MAX_MESSAGES_STML)
	{
		printk("rx_task_mgmt_fn: MAX messages exists!!!\n");
		return NULL;
	}
	*/
	#endif

	msg->next	= NULL;
	msg->prev	= NULL;
	msg->device	= dev;
	msg->value	= value;
	msg->message	= fn;

	spin_lock_irqsave (&target_data.msg_lock, flags);
	
	if (!target_data.msgq_start)
		target_data.msgq_start	= target_data.msgq_end = msg;
	else {
		target_data.msgq_end->next	= msg;
		target_data.msgq_end		= msg;
	}
	
	spin_unlock_irqrestore (&target_data.msg_lock, flags);

	#ifdef CONFIG_PROC_FS
	spin_lock_irqsave (&cmd_msg_number_lock, flags);
	current_msg_number++;
	spin_unlock_irqrestore (&cmd_msg_number_lock, flags);
	#endif
	
	return (0);
}


# if defined (FILEIO) || defined (GENERICIO)
/*
 * build_filp_table: builds up a table of open file descriptors.
 * INPUT: int (IOGENERIC/IOFILE)
 * OUTPUT: no of devices opened or < 0 if there is trouble
 */
static inline int build_filp_table (int type)
{
	char		*tmp;
	int		error, i;
	mm_segment_t 	old_fs;
	devset_t	*temp;
	struct file	*dev_file;
	
	target_data.fdlist		= NULL;
	for (i = 0; i < MAX_LUNS; i++) {
		if (type == IOGENERIC)
			sprintf (device, "/dev/sg%c", ((int)('a')+i));
		else if (type == IOFILE)
			sprintf (device, "scsi_disk_file_0_%d", i);
		else {
			printk ("build_filp_table: type unknown\n");
			return (-1);
		}
		printk ("opening device %s\n", device);
		old_fs	= get_fs(); set_fs(get_ds());
		tmp	= getname (device);
		set_fs (old_fs);
		error	= PTR_ERR (tmp);
		if (IS_ERR(tmp)) {
			printk ("scsi_target_init_module: getname returned an error %d\n", error);
			return (-1);
		}
	
		/* open a scsi_generic device */
# ifdef GENERICIO
		dev_file= filp_open(tmp, O_RDWR|O_NONBLOCK, 0);
# endif
		
# ifdef FILEIO
		dev_file= filp_open(tmp, O_RDWR|O_NONBLOCK|O_CREAT, 0);
# endif

		putname (tmp);
		if (IS_ERR (dev_file)) {
			error = PTR_ERR (dev_file);
			if (!i) {
				printk ("scsi_target_init_module: filp_open returned an error %d\n", error);
				return (-1);
			}
			else goto out_of_loop;
		}
		else {
			/*
			 * add this device to the list of open devices
			 */
			temp = (devset_t*) kmalloc (sizeof (devset_t), GFP_KERNEL);
			if (!temp) {
				printk ("sc..init: temp: kmalloc failed\n");
				return (-1);
			}

			temp->next	= NULL;
			temp->device	= dev_file;
			temp->target_id	= 0;
			temp->lun	= i << BYTE;
			temp->blk_size	= BLOCKSIZE;
			
			printk ("devices added: %s id: 0 lun: %d\n", device, i);
			if (target_data.fdlist)
				temp->next	= target_data.fdlist;
			target_data.fdlist	= temp;
		}
	}

out_of_loop:
# ifdef GENERICIO
	/* also we spawn a second thread to receive signals */
	init_MUTEX_LOCKED (&target_data.signal_sem);
	init_MUTEX_LOCKED (&target_data.sig_thr_sem);
	kernel_thread((int (*)(void *))signal_process_thread, NULL, 0);
# endif
	return 0;
}


/*
 * close_filp_table: to close all the open file descriptors
 * INPUT: None
 * OUTPUT: None
 */
static inline void close_filp_table ()
{
	devset_t	*temp;

	while (target_data.fdlist) {
		temp = target_data.fdlist;
		target_data.fdlist = target_data.fdlist->next;
		filp_close (temp->device, NULL);
		kfree (temp);
		temp = NULL;
	}
}

# endif


/*
 * get_space: allocates scatter-gather buffers for the received command
 * The assumption is that all front-ends use scatter-gather and so do
 * the back-ends ... this may change.
 * INPUT: Scsi_Request for which space has to be allocated, space needed
 * OUTPUT: 0 if everything is okay, < 0 if there is trouble
 */
static int get_space (Target_Scsi_Cmnd* cmd, unsigned int space /* in bytes */)
{
	/* We assume that scatter gather is used universally */
	struct scatterlist	*st_buffer = NULL;
	int buff_needed, i, sg_count;
	//unsigned int count=0;
	//unsigned long flags;
	int page_size = PAGE_SIZE;
	unsigned long order = 0;				

	#ifdef DISKIO

	stml_device		*stml_devp=cmd->stml_devp;
	stml_scatter_hold   *temp = NULL;
    unsigned char * stml_buf = NULL;
	//int				num =0;
	//int				rem = space;
    //
    if (cmd == NULL)
    {
        printk("error when get_space : Null cmd!\n");
        return -1;
    }
	if(!stml_devp)
	{
		return -1;
	}
	#endif

	/* should not bigger than it */
	if(space >MAX_SPACE)
	{
		printk("get_space:sorry,it's too larget %d,set to the %d\n",space,MAX_SPACE);
		space = MAX_SPACE;
	}
	/* we assume that all buffers are split by page size */

	/* get enough scatter gather entries */
	buff_needed = space/page_size;
	if (space > (buff_needed * page_size))
		buff_needed++;

	/*ramesh - added check for allocating memory*/
	if ( buff_needed == 0 )
		buff_needed = 1;


	/*try to use the buffer of the device*/

	#ifdef DISKIO

	/*if(buff_needed<=PAGES_PER_DEVICE)
	{
		spin_lock_irqsave (&stml_devp->get_mem, flags);

		for(i=0;i<BUFFER_PER_DEVICE;i++)
		{
			temp=&(stml_devp->reserve[i]);
			if(temp->used==0)
			{
				temp->used ++;
                temp->cmd_id = cmd->id;
				st_buffer = (struct scatterlist*)temp->scatterlist_buf;
				
				cmd->sglist = st_buffer;
                cmd->use_sg = buff_needed;
                cmd->sglist_len = buff_needed * sizeof(struct scatterlist);
                cmd->buf_len = buff_needed * PAGE_SIZE;
                stml_devp->cmd_use_res ++;

                spin_unlock_irqrestore(&stml_devp->get_mem, flags);
#ifdef DEBUG_HANDLE_CMD
                printk("Find reserved buffer!\n");
#endif
                
                return 0;
            }
		}

		spin_unlock_irqrestore (&stml_devp->get_mem, flags);
	}*/

	#endif

	/*
	*the scsi_raid card can handle max pages is (128-12)/2,about 60 pages.
	*/


    sg_count = 0;
    while (buff_needed >= MAX_PAGES_PER_BUFFER) {
        sg_count ++;
        buff_needed -= MAX_PAGES_PER_BUFFER;
    }
    
    order = 8;

    if (buff_needed)
        sg_count ++;

    while (buff_needed && buff_needed <= (1 << (order-1)))
        order --;

	st_buffer = (struct scatterlist*) kmalloc(sg_count * sizeof (struct scatterlist), GFP_KERNEL|GFP_ATOMIC);
	if (!st_buffer) {
		printk ("get_space: no space for st_buffer\n");
		return (-1);
	}
    sg_init_table(st_buffer, sg_count);

    temp = &(cmd->reserve);
	/* get necessary buffer space */
    for (i = 0;i < sg_count - 1; i++)
    {
        stml_buf = (unsigned char*) __get_free_pages (/*GFP_DMA |*/ GFP_KERNEL, 8);

		if(!stml_buf)
		{
			printk("get space:get_free_pages error.\n");
			return -1;
		}
		
        sg_set_buf(&st_buffer[i], (void *)stml_buf, PAGE_SIZE * MAX_PAGES_PER_BUFFER);
        temp->buf[i] = stml_buf; 
        temp->order[i] = 8;

# ifdef DEBUG_HANDLE_CMD
        printk ("st_buffer[%d] order = %ld\n", i, order);
# endif
	}
    stml_buf = (unsigned char *)__get_free_pages(GFP_KERNEL | GFP_ATOMIC, order);
        
	if(!stml_buf)
	{
		printk("get space:get_free_pages error.\n");
		return -1;
    }
    sg_set_buf(&st_buffer[sg_count - 1], (void *)stml_buf, PAGE_SIZE * (1 << order));
    temp->buf[sg_count - 1] = stml_buf;
    temp->order[i] = order;

# ifdef DEBUG_HANDLE_CMD
        printk ("st_buffer[%d] order = %ld\n", i, order);
# endif


	/*while(buff_needed>PAGES_PER_DEVICE) {
		order++;
		page_size *= 2;
		buff_needed = space/page_size;
		if(space>(buff_needed*page_size))
			buff_needed ++;
	}

	if(order>=9)  //max in kernel 
	{
		printk("get space:space  %d is too large!!!\n",space);
		return -1;
	}

	
	st_buffer = (struct scatterlist*) kmalloc(buff_needed * sizeof (struct scatterlist), GFP_KERNEL|GFP_ATOMIC);
	if (!st_buffer) {
		printk ("get_space: no space for st_buffer\n");
		return (-1);
	}
    sg_init_table(st_buffer, buff_needed);

    temp = &(cmd->reserve);
	// get necessary buffer space
    for (i = 0, count=space; i < buff_needed; i++, count-=page_size)
    {
        stml_buf = (unsigned char*) __get_free_pages (GFP_KERNEL | GFP_DMA, order);

		if(!stml_buf)
		{
			printk("get space:get_free_pages error.\n");
			return -1;
		}
		
        sg_set_buf(&st_buffer[i], (void *)stml_buf, page_size);
        temp->buf[i] = stml_buf;
        

# ifdef DEBUG_HANDLE_CMD
        //printk ("st_buffer[%d] order = %ld\n", i, order);
# endif
	}*/

	temp->scatterlist_buf = st_buffer;
    temp->page_size = page_size;
    temp->used = 1;
    temp->cmd_id = cmd->id;
    temp->k_use_sg = sg_count;
    temp->sglist_len = sg_count * sizeof(struct scatterlist);

	cmd->buf_len = (sg_count - 1) * MAX_PAGES_PER_BUFFER * PAGE_SIZE + (1 << order) * PAGE_SIZE;
	cmd->sglist = st_buffer;
	cmd->sglist_len = sg_count * sizeof( struct scatterlist );
	cmd->use_sg = sg_count;

	use_mem += cmd->buf_len;
	
#ifdef DISKIO
	stml_devp->current_data += cmd->buf_len;
#endif

//printk("get space:opcode:%2xh id:%d space:%d st_buffer:%p order:%ld sr_use_sg:%d\n",
	//cmd->cmd[0],cmd->id,space,st_buffer,order,buff_needed);
	
	return (0);
}

/*
*free space for target_cmd
*
*/
static int free_tmd(Target_Scsi_Cmnd * cmd_copy)
{
    struct scatterlist * st_list=NULL;
    int i=0;
    //int order = 0;
    //int page_size = PAGE_SIZE;
    
    stml_device *stml_devp;
    stml_scatter_hold *temp;
    unsigned long flags;


    if(!cmd_copy)
        return -1;

    // warning : for test!!!
    return 0;

# ifdef DISKIO
    
    stml_devp = cmd_copy->stml_devp;
    st_list = cmd_copy->sglist;
    
    
    if((stml_devp!=NULL)&&(st_list!=NULL)) {
        //printk("free cmnd:try to know if use the device buffer.id:%d st_list:%p\n",cmd_copy->id,st_list);
        //stml_devp->current_data -=cmd_copy->req->sr_bufflen;
        
        spin_lock_irqsave (&stml_devp->get_mem, flags);
        
        /*for(i=0;i<BUFFER_PER_DEVICE;i++)
        {
            temp=&(stml_devp->reserve[i]);
            if((temp->used) && (temp->cmd_id == cmd_copy->id) && (temp->scatterlist_buf == st_list))
            {
                temp->used --;
                temp->cmd_id = 0;
                //printk("\tfree cmnd:USEDEVICE device buf:cmdid:%d buf:%p reserve:%d\n",cmd_copy->id,st_list,i);
                cmd_copy->sglist = NULL;
                cmd_copy->buf_len = 0;
                cmd_copy->use_sg = 0;
                break;
            }
            // printk("\tfree_cmd:nouse?flag%d tempid:%d cmdid:%d res:%d\n ",temp->used,
            // temp->cmd_id,cmd_copy->id,i);
        }*/
        
        spin_unlock_irqrestore (&stml_devp->get_mem, flags);
    } 
    else
    {
        if(stml_devp==NULL)
        {
            // printk("free_cmnd:strange,stml_devp is NULL.cmdid:%d  opcode:%2xh\n",cmd_copy->id,cmd_copy->cmd[0]);
            printk("THIS INFORMATION SHOULT NOT EXIST!!!\n");
            stml_devp = target_data.devlist;

            if(stml_devp==NULL)
            {
                printk("the target data devp is NULL\n");
            } 
            else
            {
                printk("the target data devp is all right,scsi_device%p\n",stml_devp->devp);
            }
        }
        else
        {
            //printk("free_cmnd:strange,st_list is NULL. cmdid:%d  opcode:%2xh\n",cmd_copy->id,cmd_copy->cmd[0]);
        }
    }


    /* free up pages */

    //if((i == BUFFER_PER_DEVICE) && (st_list!=NULL))
    if((st_list!=NULL))
    {
        temp = &(cmd_copy->reserve);
        //    printk("free cmd:the cmd didn't use the device buf.id: %d length:%d\n",cmd_copy->id,length);
        stml_devp->current_data -= cmd_copy->buf_len;
        use_mem -= cmd_copy->buf_len;

        // printk("\tfree cmd:cmdid:%d st_buffer:%p order:%d  sr_use_sg:%d\n",
        // cmd_copy->id,st_list,order,cmd_copy->req->sr_use_sg);

        for (i = 0; i < temp->k_use_sg; i++)
        {
            free_pages ((unsigned long)temp->buf[i], temp->order[i]);
            //printk("free page for cmd %d, order %d\n", cmd_copy->id, temp->order[i]);
        }

        /* free up scatterlist */
        if (cmd_copy->use_sg)
            kfree (st_list); 
    }
    
    /* free up Scsi_Request */

# else
    // by netbear : should never be used:
    /*
    for (i = 0; i < cmd_copy->req->sr_use_sg; i++)
        free_page ((long int)st_list[i].address);
    
    // free up scatterlist
    if (cmd_copy->req->sr_use_sg)
        kfree (st_list);
    kfree (cmd_copy->req);
    */
# endif
    
    // free up Target_Scsi_Cmnd
    kfree (cmd_copy);

    return 0;
}

static int time_out(Target_Scsi_Cmnd * cmd)
{
    cmd->timeout = jiffies + 120*40*HZ;
    printk("command time out:id=%d op=%2xh\n",cmd->id,cmd->cmd[0]);

    //cmd->state = ST_DEQUEUE;
    
    /*printk("command information:
            state:%d
            abort_code:%d
            id:%d
            target_id:%d
            lun:%d
            cmd:%2x %2x %2x %2x %2x %2x %2x %2x %2x %2x
            len:%d
            cmd_read_buffer:%d\n",
            cmd->state,cmd->abort_code,
            cmd->id,(int)cmd->target_id,(int)cmd->lun,
            cmd->cmd[0],cmd->cmd[1],cmd->cmd[2],cmd->cmd[3],
            cmd->cmd[4],cmd->cmd[5],cmd->cmd[6],cmd->cmd[7],
            cmd->cmd[8],cmd->cmd[9],
            cmd->len,cmd->cmnd_read_buffer);*/

    return 0;
}

/*
 * get_allocation_length: This function looks at the received command
 * and calculates the size of the buffer to be allocated in order to
 * execute the command.
 * INPUT: pointer to command received
 * OUTPUT: buffer needing to be allocated or < 0 if there is an error
 */
static __u32 get_allocation_length (unsigned char* cmd, unsigned int type)
{
	__u32 err = 0;
	
	switch (cmd[0]) {
		
		case MODE_SENSE:
		case MODE_SELECT:
		case REQUEST_SENSE:
		{
			err = cmd[ALLOC_LEN_6];
# ifdef DEBUG_ALLOCN_LEN
			printk ("get_allocation_length: cmd  %2x  length %d\n", cmd[0],err);
# endif
			break;
		}
		
		case WRITE_10:
		case READ_10:
		case VERIFY:
		case WRITE_VERIFY:
		{
			err =	(cmd[ALLOC_LEN_10]  <<BYTE) +
			 	 cmd[ALLOC_LEN_10+1];
			err *= BLOCKSIZE;
# ifdef DEBUG_ALLOCN_LEN
			printk ("get_allocation_length: cmd  %2x  length %d\n", cmd[0],err);
# endif
			break;
		}
		
		
		case READ_DEFECT_DATA:
		case READ_LONG:
		case WRITE_LONG:
		case LOG_SELECT:
		case LOG_SENSE:
		case PERSISTENT_RESERVE_OUT:
		case PERSISTENT_RESERVE_IN:
		case MODE_SELECT_10:
		case MODE_SENSE_10:
		{
			err =	(cmd[ALLOC_LEN_10]  <<BYTE) +
			 	 cmd[ALLOC_LEN_10+1];
# ifdef DEBUG_ALLOCN_LEN
			printk ("get_allocation_length: cmd  %2x  length %d\n", cmd[0],err);
# endif
			break;
		}
		/* cdeng, August 24 2002, Report luns */
		case REPORT_LUNS:
		{
			err = (cmd[6] << 24) + (cmd[7] << 16) + (cmd[8] << 8) + cmd[9];

# ifdef DEBUG_ALLOCN_LEN
                        printk ("get_allocation_length: cmd  %2x  length %d\n", cmd[0],err);
# endif
                      break;
		}

		case READ_12:
		case WRITE_12:
		case WRITE_VERIFY_12:
		{
			err = (cmd[6] << 24) + (cmd[7] << 16) + (cmd[8] << 8) + cmd[9];

			err *= BLOCKSIZE;

# ifdef DEBUG_ALLOCN_LEN
                        printk ("get_allocation_length: cmd  %2x  length %d\n", cmd[0],err);
# endif
                      break;
		}

		case READ_BUFFER:
		{
			if(cmd[1]==0x02)
			{
				err = (cmd[6]<<16) + (cmd[7]<<8) + cmd[8];
			}else if(cmd[1]==0x00){
				err = 4;
			}else if(cmd[1]==0x03){
				err = cmd[8];
				if(err>4) err=4;
			}else if(cmd[1]==0x0b){
				err = 4;
			}else if(cmd[1]==0x0a){
				err = (cmd[6]<<16) + (cmd[7]<<8) + cmd[8];
			} else {
				err = (cmd[6]<<16) + (cmd[7]<<8) + cmd[8];
			}
# ifdef DEBUG_ALLOCN_LEN
                        printk ("get_allocation_length: cmd  %2x  length %d\n", cmd[0],err);
# endif
			break;	
		}
		case WRITE_BUFFER:
		{
			if(cmd[1]==0x00||cmd[1]==0x07){
				err = (cmd[6]<<16) + (cmd[7]<<8) + cmd[8];
			} else if(cmd[1]==0x02||cmd[1]==0x06||cmd[1]==0x0a){
				err = (cmd[6]<<16) + (cmd[7]<<8) + cmd[8];
			} else {
				err =0;
			}
			
# ifdef DEBUG_ALLOCN_LEN
                        printk ("get_allocation_length: cmd  %2x  length %d\n", cmd[0],err);
# endif
			break;
		}
		
		case INQUIRY:
		case SEND_DIAGNOSTIC:
		case RECEIVE_DIAGNOSTIC:
		{
			err = (cmd[3]<<8) + cmd[4];
# ifdef DEBUG_ALLOCN_LEN
                        printk ("get_allocation_length: cmd  %2x  length %d\n", cmd[0],err);
# endif
			break;
		}
		case READ_6:
		case WRITE_6:
		if(TYPE_TAPE == type)
		{
			err =	(cmd[2]  << 16) + (cmd[3] << 8) + cmd[4];
			if ( 1 == cmd[1] )
			{
				err *= BLOCKSIZE; /*ramesh - need to check the block size, right now it is fixed*/
			}
# ifdef DEBUG_ALLOCN_LEN
			printk ("get_allocation_length: cmd  %2x  length %d\n", cmd[0],err);
# endif
			break;
		}else{
		
			err =	cmd[4];
			err *= BLOCKSIZE; 
# ifdef DEBUG_ALLOCN_LEN
			printk ("get_allocation_length: cmd  %2x  length %d\n", cmd[0],err);
# endif
			break;
		}

		default:
		{
			printk ("get_allocation_length: cmd  %2x  length %d\n", cmd[0],err);
			break;
		}
	}
	return err;
}

/*
 * hand_to_front_end: the scsi command has been tackled by the mid-level
 * and is now ready to be handed off to the front-end either because it
 * is done or because data is needed to process this command. 
 * This function can be inline. It is separated only because it makes 
 * the main code a lot cleaner.
 * INPUT: Scsi_Cmnd that needs to be handed off
 * OUTPUT: 0 if everything is okay, < 0 if there is trouble
 */
static int hand_to_front_end (Target_Scsi_Cmnd *the_command)
{
    Scsi_Target_Device *curr_device;
# ifdef GENERICIO
    unsigned char *temp;
# endif
    
    /* get the device template corresponding to the device_id */
    for (curr_device = target_data.st_device_list; curr_device != NULL; curr_device = curr_device->next)
    {
        if (curr_device->id == the_command->dev_id)
            break;
    }
    
    if (curr_device == NULL) { /* huh - should not get here */
        /*
         * That there is not a device with this id may mean
         * one of two things - there has been a mistake or
         * that the device is no longer there. For now, the
         * former is more probable
         */
        printk ("hand_to_front_end: Could not find a device with the id matching that specified in the command (%d)\n", (int) the_command->dev_id);
        return (-1);
    }
    
    /*
     * In the time that the command was processed, it could have
     * been aborted/released. We need to check this. If so, then
     * the command state is changed to ST_DEQUEUE and returned
     */
    if (the_command->abort_code != CMND_OPEN) {
        the_command->state = ST_DEQUEUE;
        return (0);
    }

    if (the_command->state == ST_DONE) {
# ifdef DEBUG_SCSI_THREAD
        printk ("hand_to_front_end: to call xmit_response for %p id: %d\n", the_command, the_command->id);
# endif
        if ((curr_device->template) && (curr_device->template->xmit_response))
        {
            /*
             * if it is generic then we need to get the
             * blocksize information from the response to
             * the READ_CAPACITY
             */
# ifdef GENERICIO
            // by netbear : should nerver reach here
            switch (the_command->cmd[0]) {
                case READ_CAPACITY:
                {
                    temp = (unsigned char*) ((struct scatterlist*) the_command->req->sr_buffer)->address;
                    the_command->fd->blk_size = 
                    (temp[4] << (BYTE*3)) +
                    (temp[5] << (BYTE*2)) +
                    (temp[6] << (BYTE)) +
                    temp[7];
                    //printk ("READ CAPACITY: %d\n", the_command->fd->blk_size);
                    break;
                }
                
                default:
                    break;
            }
# endif
            
            if (curr_device->template->xmit_response (the_command))
            {
                printk ("hand_to_front_end: xmit_response error\n");
                return (-1);
            }

            if(the_command->state != ST_DEQUEUE)
            {
                the_command->state = ST_HANDED;
            }
        }
        else {
            printk ("hand_to_front_end: no xmit_response function in the template\n");
            return (-1);
        }
    }

    else if (the_command->state == ST_PENDING) {
# ifdef DEBUG_SCSI_THREAD
        printk ("hand_to_front_end: to call rdy_to_xfer for %p id: %d\n", the_command, the_command->id);
# endif
        if ((curr_device->template) && (curr_device->template->rdy_to_xfer))
        {
            if (curr_device->template->rdy_to_xfer (the_command))
            {
                printk ("hand_to_front_end: rdy_to_xfer error\n");
                return (-1);
            }
            if (the_command->state == ST_PENDING)
                the_command->state = ST_XFERRED;
        }
        
        else {
            printk ("hand_to_front_end: no rdy_to_xfer function in the template\n");
            return (-1);
        }
    }

    else {
            printk ("hand_to_front_end: command %p id: %d state: %d should not have reached this function\n", the_command, the_command->id, the_command->state);
            return (-1);
    }
    
    return (0);
}

/*
 * abort_notify: This function is used to notify to the low-level driver
 * that a command has been successfully aborted
 * INPUT: Target_Scsi_Cmnd
 * OUTPUT: none
 */
static inline void abort_notify (Target_Scsi_Message *msg)
{
	Target_Scsi_Cmnd *cmnd;
	
	if (msg && msg->value) {
		cmnd = (Target_Scsi_Cmnd*) msg->value;
		if (cmnd->dev_template && cmnd->dev_template->task_mgmt_fn_done)
			cmnd->dev_template->task_mgmt_fn_done (msg);
		else
			printk ("abort_notify: Unable to notify low-level driver about abort notification\n");
	}

	else
		printk ("abort_notify: Unable to notify low-level driver about abort notification\n");
}


/*
 * aen_notify: This function is used to notify to the low-level driver
 * that task management function has been received and that they need to
 * contact all initiators logged in to them and let them know. NOTE: the
 * aen_notify will also call the device that sent the task management
 * function - it needs to inform other initiators (than the one that
 * originated the Task Management function) about the received Mgmt fn
 * INPUT: management function and lun
 * OUTPUT: None
 */
static inline void aen_notify (int fn, __u64 lun)
{
	Scsi_Target_Device *dev;

	for (dev = target_data.st_device_list; dev != NULL; dev = dev->next)
	{
		if (dev && dev->template && dev->template->report_aen)
			dev->template->report_aen (fn, lun);
		else printk ("aen_notify: Unable to notify device %d\n", (int) dev->id);
	}
}




/********************************************************************
 * THESE ARE FUNCTIONS WHICH ARE SPECIFIC TO DISK IO - I lump them  *
 * 	together just to help me keep track of what is where	    *
 *******************************************************************/

#ifdef DISKIO

/*static int scan_dev_type(const char * leadin,My_scsi_idlun* my_idlun, int * host_no)
{
    char		*tmp;
    int		error;
    mm_segment_t 	old_fs;
    struct file	*dev_file;
    old_fs = get_fs();
    set_fs(get_ds());
    tmp	= getname (leadin);
    set_fs (old_fs);
    error = PTR_ERR (tmp);
    
    if (IS_ERR(tmp)) {
        printk ("scan_dev_type: getname returned an error %d\n", error);
        return error;
    }
    
    dev_file= filp_open(tmp, O_RDONLY | O_NONBLOCK, 0);
    printk("open filp : %s\n", tmp);
    putname (tmp);
    
    if (IS_ERR (dev_file)) {
        error = PTR_ERR (dev_file);
        printk ("scan_dev_type: dev_file error %d %s\n", error,tmp);
        return error;
    }

    if (dev_file->f_op == NULL)
        printk("f_op null!\n");
    else {
        if (dev_file->f_op->ioctl == NULL)
            printk("f_op->ioctl null!\n");
        if (dev_file->f_op->read == NULL)
            printk("f_op->read null\n");
    }
    if ((dev_file) && (dev_file->f_op) && (dev_file->f_op->ioctl)) {
        old_fs = get_fs();
        set_fs (get_ds());
        error = dev_file->f_op->ioctl (dev_file->f_dentry->d_inode, dev_file, SCSI_IOCTL_GET_IDLUN,(unsigned long) (my_idlun));
        if (error < 0) {
            printk ("scan_dev_type: ioctl error %d\n", error);
            return error;
        }
        set_fs (old_fs);
    }

    if ((dev_file) && (dev_file->f_op) && (dev_file->f_op->ioctl)) {
        old_fs = get_fs(); set_fs (get_ds());
        error = dev_file->f_op->ioctl (dev_file->f_dentry->d_inode, dev_file, SCSI_IOCTL_GET_BUS_NUMBER,(unsigned long)( host_no));  
       
        printk("ioctl : %d\n", *host_no);
        if (error < 0) {
            printk ("scan_dev_type: ioctl error %d\n", error);
            return error;
        }
        set_fs (old_fs);
    }
    
    error = filp_close (dev_file, NULL);
    if( error<0 ) {
        printk ("scan_dev_type: filp_close error %d\n", error);
        return error;
    }
    
    return 0;
}*/


static void make_dev_name(char * fname, const char * leadin, int k, 
                          int do_numeric)
{
    char buff[64];
    int  big,little;
    static int no = 0;

    strcpy(fname, leadin ? leadin : "/dev/sg");
    if (do_numeric) {
        sprintf(buff, "%d", k);
        strcat(fname, buff);
    }
    else {
        if (no < 26) {
            buff[0] = 'a' + (char)no;
            buff[1] = '\0';
            strcat(fname, buff);
        }
        else if (no <= 255) { /* assumes sequence goes x,y,z,aa,ab,ac etc */
            big    = no/26;
            little = no - (26 * big);
            big    = big - 1;

            buff[0] = 'a' + (char)big;
            buff[1] = 'a' + (char)little;
            buff[2] = '\0';
            strcat(fname, buff);
        }
        else
            strcat(fname, "xxxx");
        no ++;
    }
}

int	get_device_name(stml_device *  devp)
{
	//int	k;
	//unsigned char	temp[DEVICE_NAME_LENGTH];
	//My_scsi_idlun my_idlun;
    //int host_no = -1;

	if(devp == NULL)
		return 0;
    
    // by netbear: since scan_dev_type does not work , we will simply create the name of the 
    //   device ourselves. So the name may not be identical with the name under /dev
    if (TYPE_DISK == devp->type)
        make_dev_name(devp->target_device_name, "/dev/sd", devp->id, 0);
    else if (TYPE_TAPE == devp->type)
        make_dev_name(devp->target_device_name, "/dev/st", devp->id, 0);

    printk("device %s : host %d id %d lun %d channel %d\n",devp->target_device_name, devp->host_no, devp->id,
            devp->lun, devp->channel);
	/*for(k=0;k<MAX_DEVICES;k++)
	{
		if(TYPE_DISK == devp->type )
		{
			make_dev_name(temp, "/dev/sd", k, 0);
		}
		else if (TYPE_TAPE == devp->type )
		{
			make_dev_name(temp, "/dev/st", k, 1);
		} else {
			printk("get_device_name:Unkown type device!\n");
			return 0;
		}
		
		/if(scan_dev_type(temp,&my_idlun,&host_no)!=0)
		{
			printk("get_device_name:error.%s\n",temp);
			strcpy(devp->target_device_name,"Unkown");
		}

        printk("device %s : host %d id %d lun %d channel %d\n",temp, host_no, my_idlun.dev_id & 0xff,
               my_idlun.dev_id >> 8, my_idlun.dev_id >> 16);

                
    	if ((host_no == devp->host_no) && ((my_idlun.dev_id & 0xff) == devp->id) &&
	        (((my_idlun.dev_id >> 8) & 0xff) == devp->lun) &&
	        (((my_idlun.dev_id >> 16) & 0xff) == devp->channel))
        {
            if(TYPE_DISK == devp->type )
                make_dev_name(devp->target_device_name,"/dev/sd",k,0);
			else if(TYPE_TAPE == devp ->type)
                make_dev_name(devp->target_device_name,"/dev/st",k,1);
    			return 1;
        } 
	}*/
	return 0;
}

int get_device_buffer(stml_device * devp)
{
    stml_scatter_hold   *temp;
    int j=0;
    // int i;
    //unsigned char * stml_buf = NULL;

    if(!devp)
        return -1;

    for(j=0;j<BUFFER_PER_DEVICE;j++)
    {

        temp=&devp->reserve[j];
#if 0 
        temp->scatterlist_buf = (struct scatterlist*) kmalloc(PAGES_PER_DEVICE* sizeof (struct scatterlist), GFP_KERNEL|GFP_ATOMIC);
        
        printk("the point if reserve%d:%p\n",j,temp->scatterlist_buf);

        if (!temp->scatterlist_buf) {
            printk ("get_device_buffer: no space for st_buffer\n");
            temp->used = 1;
            return (-1);
        }

        /* get necessary buffer space */
        for (i = 0; i < PAGES_PER_DEVICE; i++)
        {
            stml_buf = (unsigned char*) __get_free_pages (/*GFP_DMA |*/ GFP_KERNEL,0);

            if(!(stml_buf))
            {
                printk("get_device_buf:no free pages.\n");
                temp->used = 1;
                return (-1);
            }
            
            sg_init_one(&temp->scatterlist_buf[i], (void *)stml_buf, PAGE_SIZE);
            temp->buf[i] = stml_buf;

            //printk("get_device_buf:get free pages %p\n",temp->scatterlist_buf[i].address);
        }
#endif

        temp->page_size = PAGE_SIZE;
        temp->used = 1;
        temp->cmd_id = 0;
        temp->sglist_len = PAGES_PER_DEVICE*sizeof( struct scatterlist );
        temp->k_use_sg = PAGES_PER_DEVICE;

        //debug information
        printk("get buffer for the stml device:%s  reserve%d\n",devp->target_device_name,j);
        //printk("page size:%d,used%d,sglist_len:%d,k_use_sg:%d list_buf:%p\n",
        //    (devp->reserve[j]).page_size,
        //    (devp->reserve[j]).used,
        //    (devp->reserve[j]).sglist_len,
        //    (devp->reserve[j]).k_use_sg,
        //    (devp->reserve[j]).scatterlist_buf);
    }
    
    return 0;
}

int release_device_buffer(stml_device * devp)
{
    //stml_scatter_hold   *temp;
    //int i,j;

    if(!devp)
        return -1;
#if 0
    for(j=0;j<BUFFER_PER_DEVICE;j++)
    {
        temp=&devp->reserve[j];

        for (i = 0; i < temp->k_use_sg; i++)
            free_pages ((unsigned long)temp->buf[i],0);

        /* free up scatterlist */
        kfree (temp->scatterlist_buf);
        printk("free buffer for the stml device:%s  reserve%d\n",devp->target_device_name,j);
    }
#endif
    return 0;
}

/*
 * fill_scsi_device: This function is responsible for filling in the
 * details of the device that will be used by the Target Emulator to
 * deal with the SCSI commands. If no SCSI devices are found attached
 * to the system then an error is returned
 * INPUT: NONE
 * OUTPUT: -1  for error
 */
static inline int fill_scsi_device (void)
{
    struct Scsi_Host *the_host = NULL;
    Scsi_Device *the_device = NULL;
    stml_device *stml_devp,*current_devp=NULL;
    unsigned long flags;
    int sys;
    int i = 0;

    spin_lock_irqsave (&target_data.add_delete_device, flags);
    
    /* check out the first HBA */
    sys = 0;
    for (i = 0; i < MAX_HOST; i ++) {
        the_host = scsi_host_lookup(i);
        if (the_host == NULL)
            continue;
        printk("Found Scsi Host %d\n", i);
        shost_for_each_device(the_device,the_host) {
#ifdef PROTECT_SYSTEM_DISK
            if(sys==0){
            // if(the_device)the_device=the_device->next;//just for the test
                sys=1;
            }
# endif
            printk("found device with type %d\n", the_device->type);
        /* check out the first device on this HBA */
# ifdef TAPE_DEVICE_ONLY
            if (TYPE_TAPE == the_device->type )
# else
            if (TYPE_DISK == the_device->type ||TYPE_TAPE == the_device->type )
# endif
            {
                stml_devp = (stml_device*) kmalloc (sizeof (stml_device), GFP_KERNEL);
                if (!stml_devp) {
                    printk ("fill_scsi_device: kmalloc failed\n");
                    return (-1);
                }
                stml_devp->host_no = the_device->host->host_no;
                stml_devp->id  = the_device->id;
                stml_devp->lun = the_device->lun;
                stml_devp->channel = the_device->channel;
                stml_devp->stml_id = target_data.device_num;
                printk("Found scsi device : %d %d %d\n", stml_devp->id, stml_devp->lun, stml_devp->channel);
                /* warning by netbear : I am not sure whether we should hold this pointer,
                 * since kernel code will release the reference to this pointer at the end of the loop
                 */
                stml_devp->devp = the_device;
                stml_devp->next = NULL;
                stml_devp->cmd_number = 0;
                stml_devp->cmd_use_res = 0;
                stml_devp->current_data = 0;
                stml_devp->get_mem = SPIN_LOCK_UNLOCKED;
                stml_devp->type = the_device->type;

                get_device_name(stml_devp);
                get_device_buffer(stml_devp);
                target_data.device_num ++;
                
                if(current_devp == NULL) {
                    target_data.devlist = stml_devp;
                    current_devp = stml_devp;
                }
                else {
                    current_devp->next = stml_devp;
                    current_devp = current_devp->next;
                }
            } // end if
        } // end shost_for_each_device
        scsi_host_put(the_host);
    }

    spin_unlock_irqrestore (&target_data.add_delete_device, flags);

    if (target_data.device_num == 0) {
        printk ("fill_scsi_device: could not find a SCSI device on the system .. this mode of operation is doomed for failure bailing out\n");
        return (-1);
    } 
    return (0);
}

/*
 * close_scsi_device:
 * INPUT: NONE
 * OUTPUT: -1  for error
 */
static inline int close_scsi_device (void)
{
	stml_device		*stml_devp;
	unsigned long flags;

	spin_lock_irqsave (&target_data.add_delete_device, flags);

	while(target_data.devlist) {
		stml_devp = target_data.devlist;
		target_data.devlist = stml_devp->next;
		target_data.device_num--;
		release_device_buffer(stml_devp);
		kfree(stml_devp);
		stml_devp = NULL;
	}

	spin_unlock_irqrestore (&target_data.add_delete_device, flags);

	return 1;
}

#endif

/*
 * handle_cmd: This command is responsible for dealing with the received
 * command when a real is used by the scsi target emulator. It receives
 * new commands, and gets them to the state where they are processing or
 * pending.
 * INPUT: Target_Scsi_Cmnd to be handled
 * OUTPUT: 0 if everything is okay, < 0 if there is trouble
 */
static int handle_cmd (Target_Scsi_Cmnd *cmnd)
{
	int err = 0;
	__u32 to_read;
	unsigned int type=-1;

	if(cmnd->stml_devp)
	{
		type = cmnd->stml_devp->type;
        if (cmnd->stml_devp->devp == NULL) {
            printk("handle_cmd: cmnd->stml_devp->devp is NULL!\n");
        }
	} else {
		printk("handle_cmnd:the cmnd->stml_devp is NULL\n");
	}
	
# ifdef DEBUG_HANDLE_CMD
	printk ("Entering handle_cmd : command id %d, %llx\n",cmnd->id, cmnd->tmd_tag);
# endif
	switch (cmnd->cmd[0]) {
		case READ_CAPACITY:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("READ_CAPACITY received\n");
# endif
			/* perform checks on READ_CAPACITY - LATER */

			/* allocate sg_list and get_free_pages */
			if (get_space (cmnd, READ_CAP_LEN))
			{
				printk ("handle_command: get_space returned an error for %d\n", cmnd->id);
				err = -1;
				break;
			}
			cmnd->cmnd_read_buffer=READ_CAP_LEN;
            cmnd->data_direction = DMA_FROM_DEVICE;

			/* change status */
			cmnd->state = ST_PROCESSING;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_FROM_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[0]++;
			break;
		}
		
		case INQUIRY:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("INQUIRY received\n");
# endif			
			/* perform checks on INQUIRY - LATER */
			
			/* get length */
			to_read = get_allocation_length (cmnd->cmd,type);
			if (to_read < 0) {
				printk ("handle_command: get_allocation length returned an error for %d\n", cmnd->id);
				err = -1;
				break;
			}
			
			/* allocate space */
			if (get_space (cmnd, to_read)) {
				printk ("handle_command: get_space returned an error for %d\n", cmnd->id);
				err = -1;
				break;
			}
			cmnd->cmnd_read_buffer = to_read;
            cmnd->data_direction = DMA_FROM_DEVICE;
			
			/* change status */
			cmnd->state = ST_PROCESSING;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
#ifdef DEBUG_HANDLE_CMD
            //printk("cmnd %p, stml->devp %p, cmnd->sglist %p\n", cmnd, cmnd->stml_devp->devp, cmnd->sglist);
#endif
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_FROM_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[1]++;
			break;
		}

        case REPORT_LUNS:        // cdeng        July 29, 2002
        {
# ifdef DEBUG_HANDLE_CMD
            printk ("REPORT_LUNS received\n");
# endif
            /* perform checks on REPORT_LUNS - LATER */

            /* set data direction */
            //cmnd->req->sr_data_direction = SCSI_DATA_READ;
            //cmnd->req->sr_sense_buffer[0] = 0;

            /* get length */
            to_read = get_allocation_length (cmnd->cmd,type);
            if (to_read < 0) {
                printk ("handle_command: get_allocation length returned an error for %d\n", cmnd->id);
                err = -1;
                break;
            }

            /* allocate space */
            if (get_space (cmnd, to_read)) {
                printk ("handle_command: get_space returned an error for %d\n", cmnd->id);
                err = -1;
                break;
            }
            
            cmnd->cmnd_read_buffer = to_read;
            cmnd->data_direction = DMA_FROM_DEVICE;

            /* change status */
            cmnd->state = ST_PROCESSING;

            /* hand it off to the mid-level to deal with */
            //scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            // warning by netbear : temporarily commented
			// get_report_luns_response(cmnd->req,to_read);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_FROM_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);
            err = 0;
            CommandType[2]++;
            break;
        }

		case MODE_SENSE:	// cdeng	May 28, 2002
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("MODE_SENSE received\n");
            printk ("MODE SENSE command : %d %d %d %d %d %d\n", cmnd->cmd[0], cmnd->cmd[1], cmnd->cmd[2], cmnd->cmd[3], cmnd->cmd[4], cmnd->cmd[5]);
# endif			
			/* perform checks on MODE_SENSE - LATER */
			
			/* get length */
			to_read = get_allocation_length (cmnd->cmd,type);
			if (to_read < 0) {
				printk ("handle_command: get_allocation length returned an error for %d\n", cmnd->id);
				err = -1;
				break;
			}
			
			/* allocate space */
			if (get_space (cmnd, to_read)) {
				printk ("handle_command: get_space returned an error for %d\n", cmnd->id);
				err = -1;
				break;
			}
			cmnd->cmnd_read_buffer = to_read;
            cmnd->data_direction = DMA_FROM_DEVICE;
			
			/* change status */
			cmnd->state = ST_PROCESSING;


			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY); 
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_FROM_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[3]++;
			break;
		}

		/*ramesh - 06/26/2002 WRITE FILEMARKS operation*/
		case WRITE_FILEMARKS:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("WRITE FILEMARKS received\n");
# endif
			/* perform checks on WRITE FILE MARKS */
			cmnd->state	= ST_PROCESSING;
			cmnd->cmnd_read_buffer = 0;
            cmnd->data_direction = DMA_NONE;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_NONE, NULL, 0, 0, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[4]++;
			break;
		}
		
		
		/*ramesh - 06/26/2002 SPACE operation*/
		case SPACE:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("SPACE received\n");
# endif
			/* perform checks on SPACE */
			cmnd->state	= ST_PROCESSING;
			cmnd->cmnd_read_buffer = 0;
            cmnd->data_direction = DMA_NONE;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_NONE, NULL, 0, 0, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[5]++;
			break;
		}
		
		/*ramesh - 06/26/2002 LOAD UNLOAD operation*/
		case START_STOP:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("LOAD UNLOAD received\n");
# endif
			/* perform checks on LOAD UNLOAD */
			cmnd->state		     = ST_PROCESSING;
			cmnd->cmnd_read_buffer = 0;
            cmnd->data_direction = DMA_NONE;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, START_STOP_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_NONE, NULL, 0, 0, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[6]++;
			break;
		}

		/*ramesh - 06/26/2002 LOCATE operation*/
		case SEEK_10:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("LOCATE received\n");
# endif
			/* perform checks on LOCATE */
			cmnd->state		     = ST_PROCESSING;
			cmnd->cmnd_read_buffer = 0;
            cmnd->data_direction = DMA_NONE;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_NONE, NULL, 0, 0, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[7]++;
			break;
		}
		
		/*ramesh - 06/26/2002 ERASE operation*/
		case ERASE:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("ERASE received\n");
# endif
			/* perform checks on ERASE */
			cmnd->use_sg = 0;
			cmnd->buf_len = 0;
			cmnd->state = ST_PROCESSING;
			cmnd->cmnd_read_buffer = 0;
            cmnd->data_direction = DMA_NONE;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_NONE, NULL, 0, 0, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[8]++;
			break;
		}
		

		/*ramesh - 06/26/2002 ALLOW MEDIUM REMOVAL operation*/
		case ALLOW_MEDIUM_REMOVAL:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("ALLOW MEDIUM REMOVALL received\n");
# endif
			/* perform checks on ALLOW MEDIUM REMOVAL */
			cmnd->use_sg = 0;
			cmnd->buf_len = 0;
			cmnd->state	= ST_PROCESSING;
			cmnd->cmnd_read_buffer = 0;
            cmnd->data_direction = DMA_NONE;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_NONE, NULL, 0, 0, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err			     = 0;
			CommandType[9]++;
			break;
		}
		
		/*ramesh - 06/26/2002 Read Position operation*/
		case PRE_FETCH: /* READ_POSITION */
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("PRE_FETCH/READ_POSITION received\n");
# endif
			/* perform checks on PRE_FETCH */

			to_read = 20;

			if(TYPE_TAPE == type)
			{

				if(0x00 == ((cmnd->cmd[0])&0x1F))
					to_read = 20;
				if(0x01 == (cmnd->cmd[0]&0x1F))
					to_read = 20;
				if(0x06 == (cmnd->cmd[0]&0x1F))
					to_read = 32;
				if(0x08 == (cmnd->cmd[0]&0x1F))
					to_read = 28;
			}
			
# ifdef DEBUG_HANDLE_CMD
			printk ("PRE_FETCH/READ_POSITION received,%d\n",to_read);
# endif

			/* allocate sg_list and get_free_pages */
			if (get_space (cmnd, to_read))
			{
				printk ("handle_command: get_space returned an error for %d\n", cmnd->id);
				err = -1;
				break;
			}
			cmnd->cmnd_read_buffer = to_read;
            cmnd->data_direction = DMA_FROM_DEVICE;

			/* change status */
			cmnd->state = ST_PROCESSING;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_FROM_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[10]++;
			break;
		}
		
		/*ramesh - 06/26/2002 Mode Select operation*/
		case MODE_SELECT:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("MODE SELECT received\n");
# endif
			if (cmnd->state	== ST_NEW_CMND){
				/* perform checks on MODE_SELECT - LATER */

				/* get length */
				to_read	= get_allocation_length (cmnd->cmd,type);
                cmnd->data_direction = DMA_TO_DEVICE;
				if (to_read < 0)
				{
					printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
					err = -1;
					break;
				}
				
				/* allocate sg_list and get_free_pages */
				if (get_space (cmnd, to_read))
				{
					printk ("handle_command: get_space returned an error for %d\n", cmnd->id);
					err = -1;
					break;
				}
				cmnd->cmnd_read_buffer = to_read;
				if(to_read)
					cmnd->state = ST_PENDING;
				else
					cmnd->state = ST_TO_PROCESS;
			}
			
			else if (cmnd->state == ST_TO_PROCESS)
			{
				cmnd->state = ST_PROCESSING;

				/* hand it off to the mid-level to deal with */
				//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
                scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_TO_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);
				CommandType[11]++;
			}
			err = 0;
			break;
		}
		
		/*ramesh - 06/25/2002 Read Block Limits operation*/
		case READ_BLOCK_LIMITS:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("READ_BLOCK_LIMITS received\n");
# endif
			/* perform checks on READ BLOCK LIMITS */

			/* allocate sg_list and get_free_pages */
			if (get_space (cmnd, 6))
			{
				printk ("handle_command: get_space returned an error for %d\n", cmnd->id);
				err = -1;
				break;
			}
			cmnd->cmnd_read_buffer = 6;
            cmnd->data_direction = DMA_FROM_DEVICE;

			/* change status */
			cmnd->state = ST_PROCESSING;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_FROM_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[12]++;
			break;
		}
		
		/*ramesh - 06/25/2002 rewind operation*/
		case REZERO_UNIT:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("REZERO_UNIT/REWIND received\n");
# endif
			/* perform checks on RESERO UNIT */
			cmnd->use_sg	     = 0;
			cmnd->buf_len	     = 0;
			cmnd->state		     = ST_PROCESSING;
			cmnd->cmnd_read_buffer = 0;
            cmnd->data_direction = DMA_NONE;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_NONE, NULL, 0, 0, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[13]++;
			break;
		}
		
		case TEST_UNIT_READY:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("TEST UNIT READY received\n");
# endif
			/* perform checks on TEST UNIT READY */
			cmnd->use_sg = 0;
			cmnd->buf_len = 0;
			cmnd->state	= ST_PROCESSING;
			cmnd->cmnd_read_buffer = 0;
            cmnd->data_direction = DMA_NONE;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_NONE, NULL, 0, 0, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[14]++;
			break;
		}

		case VERIFY:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("VERIFY received\n");
# endif
			/* perform checks on TEST UNIT READY */
			cmnd->use_sg = 0;
			cmnd->buf_len = 0;
			cmnd->state		     = ST_PROCESSING;
			cmnd->cmnd_read_buffer = 0;
            cmnd->data_direction = DMA_NONE;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_NONE, NULL, 0, 0, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err	= 0;
			CommandType[15]++;
			break;
		}

		/*ramesh - 06/27/2002 READ 6 operation*/
		case READ_6:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("READ_6 received\n");
# endif
			/* perform checks for READ_6 */
			
			/* get length */
			to_read	= get_allocation_length (cmnd->cmd,type);
			if (to_read < 0) {
				printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
				err = -1;
				break;
			}
			
			if (get_space (cmnd, to_read)) {
				printk ("handle_cmd: get_space returned an error for %d\n", cmnd->id);
				err = -1;
				break;
			}
			cmnd->cmnd_read_buffer = to_read;
            cmnd->data_direction = DMA_FROM_DEVICE;

			/* change_status */
			cmnd->state	= ST_PROCESSING;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_FROM_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);
			err = 0;
			CommandType[16]++;
			break;
		}
		
		case READ_10:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("READ_10 received\n");
            //printk ("READ_10 command : %d %d %d %d %d %d %d %d %d %d\n", cmnd->cmd[0], cmnd->cmd[1], cmnd->cmd[2], cmnd->cmd[3], cmnd->cmd[4], cmnd->cmd[5], cmnd->cmd[6], cmnd->cmd[7], cmnd->cmd[8], cmnd->cmd[9]);
# endif
			/* perform checks for READ_10 */
			
			/* get length */
			to_read	= get_allocation_length (cmnd->cmd,type);
			if (to_read < 0) {
				printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
				err = -1;
				break;
			}
			
			if (get_space (cmnd, to_read)) {
				printk ("handle_cmd: get_space returned an error for %d\n", cmnd->id);
				err = -1;
				break;
			}
			cmnd->cmnd_read_buffer = to_read;
            cmnd->data_direction = DMA_FROM_DEVICE;

			/* change_status */
			cmnd->state	= ST_PROCESSING;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_FROM_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[17]++;
			break;
		}
		

		/*ramesh - 06/27/2002 write 6 operation*/
		case WRITE_6:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("WRITE_6 received -- state - %d buff len - %d\n", cmnd->state, cmnd->buf_len);
# endif
			if (cmnd->state	== ST_NEW_CMND) {
				/* perform checks on the received WRITE_6 */
				
				/* get length */
				to_read = get_allocation_length (cmnd->cmd,type);
				if (to_read < 0) {
					printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
					err = -1;
					break;
				}

				/* get space */
				if (get_space (cmnd, to_read)) {
					printk ("handle_cmd: get_space returned error for %d\n", cmnd->id);
					err = -1;
					break;
				}
				cmnd->cmnd_read_buffer = to_read;
                cmnd->data_direction = DMA_TO_DEVICE;
				if(to_read)
					cmnd->state = ST_PENDING;
				else
					cmnd->state = ST_TO_PROCESS;
			}
			
			else if (cmnd->state == ST_TO_PROCESS) {
				cmnd->state = ST_PROCESSING;

				/* hand it off to the mid-level to deal with */
				//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
                scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_TO_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

				CommandType[18]++;
			}

			err = 0;
			break;
		}

		case WRITE_10:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("WRITE_10 received\n");
# endif
			if (cmnd->state	== ST_NEW_CMND) {
				/* perform checks on the received WRITE_10 */
				
				/* get length */
				to_read = get_allocation_length (cmnd->cmd,type);
				if (to_read < 0) {
					printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
					err = -1;
					break;
				}

				/* get space */
				if (get_space (cmnd, to_read)) {
					printk ("handle_cmd: get_space returned error for %d\n", cmnd->id);
					err = -1;
					break;
				}
				cmnd->cmnd_read_buffer = to_read;
                cmnd->data_direction = DMA_TO_DEVICE;
				if(to_read)
					cmnd->state = ST_PENDING;
				else
					cmnd->state = ST_TO_PROCESS;
			}
			
			else if (cmnd->state == ST_TO_PROCESS) {
				cmnd->state = ST_PROCESSING;

				/* hand it off to the mid-level to deal with */
				//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
                scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_TO_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

				CommandType[19]++;
			}

			err = 0;
			break;
		}

		case RESERVE:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("RESERVE received\n");
# endif
			/* perform checks on RESERVE */
			cmnd->use_sg = 0;
			cmnd->buf_len = 0;
			cmnd->state = ST_PROCESSING;
			cmnd->cmnd_read_buffer = 0;
            cmnd->data_direction = DMA_NONE;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_NONE, NULL, 0, 0, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[20]++;
			break;
		}

		case RELEASE:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("RELEASE received\n");
# endif
			/* perform checks on RELEASE */
			cmnd->use_sg = 0;
			cmnd->buf_len = 0;
			cmnd->state = ST_PROCESSING;
			cmnd->cmnd_read_buffer = 0;
            cmnd->data_direction = DMA_NONE;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_NONE, NULL, 0, 0, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[21]++;
			break;
		}

		case SEND_DIAGNOSTIC:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("SEND_DIAGNOSTIC received -- state - %d buff len - %d\n", cmnd->state, cmnd->buf_len);
# endif
			if (cmnd->state	== ST_NEW_CMND) {
				/* perform checks on the received SEND_DIAGNOSTIC */
				
				/* get length */
				to_read = get_allocation_length (cmnd->cmd,type);
				if (to_read < 0) {
					printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
					err = -1;
					break;
				}

				/* get space */
				if (get_space (cmnd, to_read)) {
					printk ("handle_cmd: get_space returned error for %d\n", cmnd->id);
					err = -1;
					break;
				}
				cmnd->cmnd_read_buffer = to_read;
                cmnd->data_direction = DMA_TO_DEVICE;
				if(to_read)
					cmnd->state = ST_PENDING;
				else
					cmnd->state = ST_TO_PROCESS;
			}
			
			else if (cmnd->state == ST_TO_PROCESS) {
				cmnd->state = ST_PROCESSING;

				/* hand it off to the mid-level to deal with */
				//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
                scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_TO_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

				CommandType[22]++;
			}

			err = 0;
			break;
		}

		case REQUEST_SENSE:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("REQUEST_SENSE received\n");
# endif
			/* perform checks for REQUEST_SENSE */
			
			/* get length */
			to_read	= get_allocation_length (cmnd->cmd,type);
			if (to_read < 0) {
				printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
				err = -1;
				break;
			}
			
			if (get_space (cmnd, to_read)) {
				printk ("handle_cmd: get_space returned an error for %d\n", cmnd->id);
				err = -1;
				break;
			}
			cmnd->cmnd_read_buffer = to_read;
            cmnd->data_direction = DMA_FROM_DEVICE;

			/* change_status */
			cmnd->state	= ST_PROCESSING;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_FROM_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err	= 0;
			CommandType[23]++;
			break;
		}

		case MODE_SELECT_10:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("MODE MODE_SELECT_10 received\n");
# endif
			if (cmnd->state	== ST_NEW_CMND){
				/* perform checks on MODE_SELECT_10 - LATER */

				/* get length */
				to_read	= get_allocation_length (cmnd->cmd,type);
				if (to_read < 0)
				{
					printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
					err = -1;
					break;
				}
				
				/* allocate sg_list and get_free_pages */
				if (get_space (cmnd, to_read))
				{
					printk ("handle_command: get_space returned an error for %d\n", cmnd->id);
					err = -1;
					break;
				}
				cmnd->cmnd_read_buffer = to_read;
                cmnd->data_direction = DMA_TO_DEVICE;
				if(to_read)
					cmnd->state = ST_PENDING;
				else
					cmnd->state = ST_TO_PROCESS;
			}
			
			else if (cmnd->state == ST_TO_PROCESS)
			{
				cmnd->state = ST_PROCESSING;

				/* hand it off to the mid-level to deal with */
				//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
                scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_TO_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);
				CommandType[24]++;
			}
			err	= 0;
			break;
		}


		case MODE_SENSE_10:	// cdeng	May 28, 2002
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("MODE_SENSE_10 received\n");
# endif			
			/* perform checks on MODE_SENSE_10 - LATER */
			
			/* get length */
			to_read = get_allocation_length (cmnd->cmd,type);
			if (to_read < 0) {
				printk ("handle_command: get_allocation length returned an error for %d\n", cmnd->id);
				err = -1;
				break;
			}
			
			/* allocate space */
			if (get_space (cmnd, to_read)) {
				printk ("handle_command: get_space returned an error for %d\n", cmnd->id);
				err = -1;
				break;
			}
			cmnd->cmnd_read_buffer  = to_read;
            cmnd->data_direction = DMA_FROM_DEVICE;
			
			/* change status */
			cmnd->state = ST_PROCESSING;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_FROM_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[25]++;
			break;
		}

		case RECEIVE_DIAGNOSTIC:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("RECEIVE_DIAGNOSTIC received\n");
# endif
			/* perform checks for RECEIVE_DIAGNOSTIC */
			
			/* get length */
			to_read	= get_allocation_length (cmnd->cmd,type);
			if (to_read < 0) {
				printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
				err = -1;
				break;
			}
			
			if (get_space (cmnd, to_read)) {
				printk ("handle_cmd: get_space returned an error for %d\n", cmnd->id);
				err = -1;
				break;
			}
			cmnd->cmnd_read_buffer = to_read;
            cmnd->data_direction = DMA_FROM_DEVICE;

			/* change_status */
			cmnd->state	= ST_PROCESSING;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_FROM_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[26]++;
			break;
		}

		case LOCK_UNLOCK_CACHE:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("LOCK_UNLOCK_CACHE received\n");
# endif
			/* perform checks on LOCK_UNLOCK_CACHE */
			cmnd->use_sg = 0;
			cmnd->buf_len = 0;
			cmnd->state = ST_PROCESSING;
			cmnd->cmnd_read_buffer = 0;
            cmnd->data_direction = DMA_NONE;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_NONE, NULL, 0, 0, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err			     = 0;
			CommandType[27]++;
			break;
		}

		case READ_DEFECT_DATA:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("READ_DEFECT_DATA received\n");
# endif
			/* perform checks for READ_DEFECT_DATA */
			for (err=0;err<10;err++) {
			    printk("%2x\n",cmnd->cmd[err]);
			}
			
			/* get length */
			to_read	= get_allocation_length (cmnd->cmd,type);
			if (to_read < 0) {
				printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
				err = -1;
				break;
			}
			
			if (get_space (cmnd, to_read)) {
				printk ("handle_cmd: get_space returned an error for %d\n", cmnd->id);
				err = -1;
				break;
			}
			cmnd->cmnd_read_buffer = to_read;
            cmnd->data_direction = DMA_FROM_DEVICE;

			/* change_status */
			cmnd->state	= ST_PROCESSING;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, READ_DEFECT_DATA_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_FROM_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err		= 0;
			CommandType[28]++;
			break;
		}

		case READ_LONG:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("READ_LONG received\n");
# endif
			/* perform checks for READ_LONG */
			
			/* get length */
			to_read	= get_allocation_length (cmnd->cmd,type);
			if (to_read < 0) {
				printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
				err = -1;
				break;
			}
			
			if (get_space (cmnd, to_read)) {
				printk ("handle_cmd: get_space returned an error for %d\n", cmnd->id);
				err = -1;
				break;
			}
			cmnd->cmnd_read_buffer = to_read;
            cmnd->data_direction = DMA_FROM_DEVICE;

			/* change_status */
			cmnd->state	= ST_PROCESSING;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_FROM_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[29]++;
			break;
		}

		case REASSIGN_BLOCKS:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("REASSIGN_BLOCKS received\n");
# endif
			/* perform checks on REASSIGN_BLOCKS */
			cmnd->use_sg = 0;
			cmnd->buf_len = 0;
			cmnd->state = ST_PROCESSING;
			cmnd->cmnd_read_buffer = 0;
            cmnd->data_direction = DMA_NONE;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_NONE, NULL, 0, 0, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[30]++;
			break;
		}

		case SET_LIMITS:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("SET_LIMITS received\n");
# endif
			/* perform checks on SET_LIMITS */
			cmnd->use_sg = 0;
			cmnd->buf_len = 0;
			cmnd->state = ST_PROCESSING;
			cmnd->cmnd_read_buffer = 0;
            cmnd->data_direction = DMA_NONE;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_NONE, NULL, 0, 0, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[31]++;
			break;
		}

		case SYNCHRONIZE_CACHE:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("SYNCHRONIZE_CACHE received\n");
# endif
			/* perform checks on SYNCHRONIZE_CACHE */
			cmnd->use_sg = 0;
			cmnd->buf_len = 0;
			cmnd->state = ST_PROCESSING;
			cmnd->cmnd_read_buffer = 0;
            cmnd->data_direction = DMA_NONE;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_NONE, NULL, 0, 0, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[32]++;
			break;
		}


		case WRITE_VERIFY:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("WRITE_VERIFY received\n");
# endif
			if (cmnd->state	== ST_NEW_CMND) {
				/* perform checks on the received WRITE_VERIFY */
				
				/* get length */
				to_read = get_allocation_length (cmnd->cmd,type);
				if (to_read < 0) {
					printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
					err = -1;
					break;
				}

				/* get space */
				if (get_space (cmnd, to_read)) {
					printk ("handle_cmd: get_space returned error for %d\n", cmnd->id);
					err = -1;
					break;
				}
				cmnd->cmnd_read_buffer = to_read;
                cmnd->data_direction = DMA_TO_DEVICE;
				if(to_read)
					cmnd->state = ST_PENDING;
				else
					cmnd->state = ST_TO_PROCESS;
			}
			
			else if (cmnd->state == ST_TO_PROCESS) {
				cmnd->state = ST_PROCESSING;

				/* hand it off to the mid-level to deal with */
				//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
                scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_TO_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

				CommandType[33]++;
			}
			err		= 0;
			
			break;
		}

		case READ_12:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("READ_12 received\n");
# endif
			/* perform checks for READ_12 */
			
			/* get length */
			to_read	= get_allocation_length (cmnd->cmd,type);
			if (to_read < 0) {
				printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
				err = -1;
				break;
			}
			
			if (get_space (cmnd, to_read)) {
				printk ("handle_cmd: get_space returned an error for %d\n", cmnd->id);
				err = -1;
				break;
			}
			cmnd->cmnd_read_buffer = to_read;
            cmnd->data_direction = DMA_FROM_DEVICE;

			/* change_status */
			cmnd->state	= ST_PROCESSING;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_FROM_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err	= 0;
			CommandType[34]++;
			break;
		}

		case WRITE_12:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("WRITE_12 received\n");
# endif
			if (cmnd->state	== ST_NEW_CMND) {
				/* perform checks on the received WRITE_12 */
				
				/* get length */
				to_read = get_allocation_length (cmnd->cmd,type);
				if (to_read < 0) {
					printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
					err = -1;
					break;
				}

				/* get space */
				if (get_space (cmnd, to_read)) {
					printk ("handle_cmd: get_space returned error for %d\n", cmnd->id);
					err = -1;
					break;
				}
				cmnd->cmnd_read_buffer = to_read;
                cmnd->data_direction = DMA_TO_DEVICE;
				if(to_read)
					cmnd->state = ST_PENDING;
				else
					cmnd->state = ST_TO_PROCESS;
			}
			
			else if (cmnd->state == ST_TO_PROCESS) {
				cmnd->state = ST_PROCESSING;

				/* hand it off to the mid-level to deal with */
				//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
                scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_TO_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

				CommandType[35]++;
			}
			err = 0;
			
			break;
		}

		case WRITE_VERIFY_12:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("WRITE_VERIFY_12 received\n");
# endif
			if (cmnd->state	== ST_NEW_CMND) {
				/* perform checks on the received WRITE_VERIFY_12 */
				
				/* get length */
				to_read = get_allocation_length (cmnd->cmd,type);
				if (to_read < 0) {
					printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
					err = -1;
					break;
				}

				/* get space */
				if (get_space (cmnd, to_read)) {
					printk ("handle_cmd: get_space returned error for %d\n", cmnd->id);
					err = -1;
					break;
				}
				cmnd->cmnd_read_buffer = to_read;
                cmnd->data_direction = DMA_TO_DEVICE;
				if(to_read)
					cmnd->state = ST_PENDING;
				else
					cmnd->state = ST_TO_PROCESS;
			}
			
			else if (cmnd->state == ST_TO_PROCESS) {
				cmnd->state = ST_PROCESSING;

				/* hand it off to the mid-level to deal with */
				//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
                scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_TO_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

				CommandType[36]++;
			}

			err	= 0;
			break;
		}

		case WRITE_LONG:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("WRITE_LONG received\n");
# endif
			if (cmnd->state	== ST_NEW_CMND) {
				/* perform checks on the received WRITE_LONG */
				
				/* get length */
				to_read = get_allocation_length (cmnd->cmd,type);
				if (to_read < 0) {
					printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
					err = -1;
					break;
				}

				/* get space */
				if (get_space (cmnd, to_read)) {
					printk ("handle_cmd: get_space returned error for %d\n", cmnd->id);
					err = -1;
					break;
				}
				cmnd->cmnd_read_buffer = to_read;
                cmnd->data_direction = DMA_TO_DEVICE;
				if(to_read)
					cmnd->state = ST_PENDING;
				else
					cmnd->state = ST_TO_PROCESS;
			}
			
			else if (cmnd->state == ST_TO_PROCESS) {
				cmnd->state = ST_PROCESSING;

				/* hand it off to the mid-level to deal with */
				//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
                scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_TO_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

				CommandType[37]++;
			}
			err = 0;
			
			break;
		}

		case WRITE_SAME:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("WRITE_SAME received\n");
# endif
			/* perform checks on WRITE_SAME */
			cmnd->use_sg = 0;
			cmnd->buf_len = 0;
			cmnd->state = ST_PROCESSING;
			cmnd->cmnd_read_buffer = 0;
            cmnd->data_direction = DMA_NONE;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_NONE, NULL, 0, 0, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[38]++;
			break;
		}

		case LOG_SELECT:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("LOG_SELECT received\n");
# endif
			if (cmnd->state	== ST_NEW_CMND) {
				/* perform checks on the received LOG_SELECT */
				
				/* get length */
				to_read = get_allocation_length (cmnd->cmd,type);
				if (to_read < 0) {
					printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
					err = -1;
					break;
				}

				/* get space */
				if (get_space (cmnd, to_read)) {
					printk ("handle_cmd: get_space returned error for %d\n", cmnd->id);
					err = -1;
					break;
				}
				cmnd->cmnd_read_buffer = to_read;
                cmnd->data_direction = DMA_TO_DEVICE;
				if(to_read)
					cmnd->state = ST_PENDING;
				else
					cmnd->state = ST_TO_PROCESS;
			}
			
			else if (cmnd->state == ST_TO_PROCESS) {
				cmnd->state = ST_PROCESSING;

				/* hand it off to the mid-level to deal with */
				//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
                scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_TO_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

				CommandType[39]++;
			}
			err		= 0;
			break;
		}

		case LOG_SENSE:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("LOG_SENSE received\n");
# endif
			/* perform checks for LOG_SENSE */
			
			/* get length */
			to_read	= get_allocation_length (cmnd->cmd,type);
			if (to_read < 0) {
				printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
				err = -1;
				break;
			}
			
			if (get_space (cmnd, to_read)) {
				printk ("handle_cmd: get_space returned an error for %d\n", cmnd->id);
				err = -1;
				break;
			}
			cmnd->cmnd_read_buffer = to_read;
            cmnd->data_direction = DMA_FROM_DEVICE;

			/* change_status */
			cmnd->state	= ST_PROCESSING;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_FROM_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err		= 0;
			CommandType[40]++;
			break;
		}

		case PERSISTENT_RESERVE_OUT:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("PERSISTENT_RESERVE_OUT received\n");
# endif
			if (cmnd->state	== ST_NEW_CMND) {
				/* perform checks on the received PERSISTENT_RESERVE_OUT */
				
				/* get length */
				to_read = get_allocation_length (cmnd->cmd,type);
				if (to_read < 0) {
					printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
					err = -1;
					break;
				}

				/* get space */
				if (get_space (cmnd, to_read)) {
					printk ("handle_cmd: get_space returned error for %d\n", cmnd->id);
					err = -1;
					break;
				}
				cmnd->cmnd_read_buffer = to_read;
                cmnd->data_direction = DMA_TO_DEVICE;
				if(to_read)
					cmnd->state = ST_PENDING;
				else
					cmnd->state = ST_TO_PROCESS;
			}
			
			else if (cmnd->state == ST_TO_PROCESS) {
				cmnd->state = ST_PROCESSING;

				/* hand it off to the mid-level to deal with */
				//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
                scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_TO_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

				CommandType[41]++;
			}
			err		= 0;
			break;
		}

		case PERSISTENT_RESERVE_IN:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("PERSISTENT_RESERVE_IN received\n");
# endif
			/* perform checks for PERSISTENT_RESERVE_IN */
			
			/* get length */
			to_read	= get_allocation_length (cmnd->cmd,type);
			if (to_read < 0) {
				printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
				err = -1;
				break;
			}
			
			if (get_space (cmnd, to_read)) {
				printk ("handle_cmd: get_space returned an error for %d\n", cmnd->id);
				err = -1;
				break;
			}
			cmnd->cmnd_read_buffer = to_read;
            cmnd->data_direction = DMA_FROM_DEVICE;

			/* change_status */
			cmnd->state	= ST_PROCESSING;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_FROM_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err = 0;
			CommandType[42]++;
			break;
		}

		case READ_BUFFER:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("READ_BUFFER received\n");
# endif
			/* perform checks for READ_BUFFER */
			
			/* get length */
			to_read	= get_allocation_length (cmnd->cmd,type);
			if (to_read < 0) {
				printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
				err = -1;
				break;
			}
			
			if (get_space (cmnd, to_read)) {
				printk ("handle_cmd: get_space returned an error for %d\n", cmnd->id);
				err = -1;
				break;
			}
			cmnd->cmnd_read_buffer = to_read;
            cmnd->data_direction = DMA_FROM_DEVICE;

			/* change_status */
			cmnd->state	= ST_PROCESSING;

			/* hand it off to the mid-level to deal with */
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
            scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_FROM_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

			err		= 0;
			CommandType[43]++;
			break;
		}

		case WRITE_BUFFER:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("WRITE_BUFFER received\n");
# endif
			if (cmnd->state	== ST_NEW_CMND) {
				/* perform checks on the received WRITE_BUFFER */
				
				/* get length */
				to_read = get_allocation_length (cmnd->cmd,type);
				if (to_read < 0) {
					printk ("handle_cmd: get_allocation_length returned (%d) an error for %d\n", to_read, cmnd->id);
					err = -1;
					break;
				}

				/* get space */
				if (get_space (cmnd, to_read)) {
					printk ("handle_cmd: get_space returned error for %d\n", cmnd->id);
					err = -1;
					break;
				}
				cmnd->cmnd_read_buffer = to_read;
                cmnd->data_direction = DMA_TO_DEVICE;
				if(to_read)
					cmnd->state = ST_PENDING;
				else
					cmnd->state = ST_TO_PROCESS;
			}
			
			else if (cmnd->state == ST_TO_PROCESS) {
				cmnd->state = ST_PROCESSING;

				/* hand it off to the mid-level to deal with */
				//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, TE_TIMEOUT, TE_TRY);
                scsi_execute_async(cmnd->stml_devp->devp, cmnd->cmd, cmnd->len, DMA_TO_DEVICE, cmnd->sglist, cmnd->buf_len, cmnd->use_sg, TE_TIMEOUT, TE_TRY, cmnd, te_cmnd_processed, GFP_KERNEL);

				CommandType[44]++;
			}
			err		= 0;
			break;
		}

		case FORMAT_UNIT:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("FORMAT_UNIT received\n");
# endif
			/* perform checks on FORMAT_UNIT */
			cmnd->use_sg = 0;
			cmnd->buf_len = 0;
			cmnd->state = ST_PROCESSING;
			cmnd->cmnd_read_buffer = 0;

			/* hand it off to the mid-level to deal with */
            // warning by netbear : not hand off to mid-level
			//scsi_do_req (cmnd->req, cmnd->cmd, cmnd->req->sr_buffer, cmnd->req->sr_bufflen, te_cmnd_processed, FORMAT_UNIT_TIMEOUT, TE_TRY);

			err	= 0;
			cmnd->state = ST_DONE;
			CommandType[45]++;
			break;
		}




		default:
		{
# ifdef DEBUG_HANDLE_CMD
			printk ("unknown command received:cmd[0]=%2x\n",cmnd->cmd[0]);
# endif
			err = -1;
			CommandType[46]++;
			break;
		}
	}
# ifdef DEBUG_HANDLE_CMD
	//printk ("handle_cmd: handing it off to mid_level\n");
# endif
	return (err);
}

/*
 * te_cmnd_processed: This is the done function called by the mid-level
 * upon detecting that a particular command has been executed. This
 * function can be called from an interrupt context - it is responsible
 * for changing the state of the command and waking up the target 
 * mid-level.(or failed)
 */
static void te_cmnd_processed (void * data, char * sense, int result, int datalen)
{
	Target_Scsi_Cmnd * te_cmd =  (Target_Scsi_Cmnd *)data;

	/* find the Target_Scsi_Cmnd containing the scsi_request 
	te_cmd = target_data.cmd_queue_start;
	while (te_cmd) {
		if (te_cmd->req == req)
			break;
		te_cmd	= te_cmd->next;
	}*/

	if (!te_cmd) { 
        // we can just throw away the response 
        // may be the command was aborted
		printk ("te_cmnd_processed: Could not find request in the command queue\n");
	}

	else {
		/* change its state */
        te_cmd->result = result;
        if (result)
		    printk ("Error when processing cmd %p id: %d with result %d\n", te_cmd, te_cmd->id, result);
        if (sense)
            memcpy(te_cmd->sense, sense, SCSI_SENSE_BUFFERSIZE);
        else
            printk("Error when processing cmd : no sense data\n");
        // sg_copy_to_buffer may be a sleeping function and should not be called here
#if 0
        /* cdeng, August 24 2002, only support lun zero for the disk in linux system */
        sg_copy_to_buffer(te_cmd->sglist, 1, te_sg_buf, te_cmd->reserve.page_size);

		if (te_cmd->lun && te_cmd->cmd[0] == INQUIRY)
		{
			te_sg_buf[0] = 0x7f;
		}

		if(te_cmd->cmd[0] == INQUIRY)
        {

			strcpy((char*)&te_sg_buf[8],"CLVM  ");
		}

		if(te_cmd->cmd[0] == REZERO_UNIT)
		{
			printk("is REWIND error? cmd id=%d\n",te_cmd->id);
		}

        sg_copy_from_buffer(te_cmd->sglist, 1, te_sg_buf, te_cmd->reserve.page_size);
#endif
		te_cmd->state	= ST_DONE;
#ifdef DEBUG_TE_CMD
        printk("finish processing command , id %d \n", te_cmd->id);
#endif
		up (&target_data.target_sem);
	}

	return;
}

#ifdef MODULE
MODULE_AUTHOR("Bob Russell");
MODULE_DESCRIPTION("UNH");
MODULE_SUPPORTED_DEVICE("sd");
#ifdef MODULE_LICENSE
MODULE_LICENSE("GPL");
#endif
#endif
