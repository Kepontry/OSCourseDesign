; ==========================================
; pmtest9.asm
; 编译方法：nasm pmtest9.asm -o pmtest9.com
; ==========================================

%include	"pm.inc"	; 常量, 宏, 以及一些说明

PageDirBase0		equ	200000h	; 页目录开始地址:	2M
PageTblBase0		equ	201000h	; 页表开始地址:		2M +  4K
PageDirBase1		equ	210000h	; 页目录开始地址:	2M + 64K
PageTblBase1		equ	211000h	; 页表开始地址:		2M + 64K + 4K

LinearAddrDemo	equ	00401000h
ProcFoo		equ	00401000h
ProcBar		equ	00501000h

ProcPagingDemo	equ	00301000h

org	0100h
	jmp	LABEL_BEGIN

[SECTION .gdt]
; GDT
;                                         段基址,       段界限     , 属性
LABEL_GDT:		Descriptor	       0,                 0, 0				; 空描述符
LABEL_DESC_NORMAL:	Descriptor	       0,            0ffffh, DA_DRW			; Normal 描述符
LABEL_DESC_FLAT_C:	Descriptor             0,           0fffffh, DA_CR | DA_32 | DA_LIMIT_4K; 0 ~ 4G
LABEL_DESC_FLAT_RW:	Descriptor             0,           0fffffh, DA_DRW | DA_LIMIT_4K	; 0 ~ 4G
LABEL_DESC_CODE32:	Descriptor	       0,  SegCode32Len - 1, DA_CR | DA_32		; 非一致代码段, 32
LABEL_DESC_CODE16:	Descriptor	       0,            0ffffh, DA_C			; 非一致代码段, 16
LABEL_DESC_DATA:	Descriptor	       0,	DataLen - 1, DA_DRW			; Data
LABEL_DESC_STACK:	Descriptor	       0,        TopOfStack, DA_DRWA | DA_32		; Stack, 32 位
LABEL_DESC_VIDEO:	Descriptor	 0B8000h,            0ffffh, DA_DRW			; 显存首地址
; GDT 结束

GdtLen		equ	$ - LABEL_GDT	; GDT长度
GdtPtr		dw	GdtLen		; GDT界限
		dd	0		; GDT基地址

; GDT 选择子
SelectorNormal		equ	LABEL_DESC_NORMAL	- LABEL_GDT
SelectorFlatC		equ	LABEL_DESC_FLAT_C	- LABEL_GDT
SelectorFlatRW		equ	LABEL_DESC_FLAT_RW	- LABEL_GDT
SelectorCode32		equ	LABEL_DESC_CODE32	- LABEL_GDT
SelectorCode16		equ	LABEL_DESC_CODE16	- LABEL_GDT
SelectorData		equ	LABEL_DESC_DATA		- LABEL_GDT
SelectorStack		equ	LABEL_DESC_STACK	- LABEL_GDT
SelectorVideo		equ	LABEL_DESC_VIDEO	- LABEL_GDT
; END of [SECTION .gdt]

[SECTION .data1]	 ; 数据段
ALIGN	32
[BITS	32]
LABEL_DATA:
; 实模式下使用这些符号
; 字符串
_szPMMessage:			db	"In Protect Mode now. ^-^", 0Ah, 0Ah, 0	; 进入保护模式后显示此字符串,0A代表换行,0代表字符串结尾'\0',这些参数在DispStr中会得到处理
_szMemChkTitle:			db	"BaseAddrL BaseAddrH LengthLow LengthHigh   Type", 0Ah, 0	; 进入保护模式后显示此字符串,0A代表换行,0代表字符串结尾'\0'
_szRAMSize			db	"RAM size:", 0		;在DispMemSize(显示内存信息)中被使用,0代表字符串结尾'\0'
_szReturn			db	0Ah, 0	;相当于一个换行符，在DispReturn中被使用
; 变量
_wSPValueInRealMode		dw	0
_dwMCRNumber:			dd	0	; Memory Check Result
_dwDispPos:			dd	(80 * 6 + 0) * 2	; 屏幕第 6 行, 第 0 列。用于显示下一个内容在屏幕上将要输出的位置
_dwMemSize:			dd	0
_ARDStruct:			; Address Range Descriptor Structure
	_dwBaseAddrLow:		dd	0
	_dwBaseAddrHigh:	dd	0
	_dwLengthLow:		dd	0
	_dwLengthHigh:		dd	0
	_dwType:		dd	0
_PageTableNumber:		dd	0
_SavedIDTR:			dd	0	;用于保存 IDTR
				dd	0
_SavedIMREG:			db	0	;用于保存中断屏蔽寄存器值
_MemChkBuf:	times	256	db	0

; 保护模式下使用这些符号
szPMMessage		equ	_szPMMessage	- $$
szMemChkTitle		equ	_szMemChkTitle	- $$
szRAMSize		equ	_szRAMSize	- $$
szReturn		equ	_szReturn	- $$
dwDispPos		equ	_dwDispPos	- $$	;_dwDispPos变量相对于本节开始处的偏移
dwMemSize		equ	_dwMemSize	- $$
dwMCRNumber		equ	_dwMCRNumber	- $$
ARDStruct		equ	_ARDStruct	- $$
	dwBaseAddrLow	equ	_dwBaseAddrLow	- $$
	dwBaseAddrHigh	equ	_dwBaseAddrHigh	- $$
	dwLengthLow	equ	_dwLengthLow	- $$
	dwLengthHigh	equ	_dwLengthHigh	- $$
	dwType		equ	_dwType		- $$
MemChkBuf		equ	_MemChkBuf	- $$
SavedIDTR		equ	_SavedIDTR	- $$
SavedIMREG		equ	_SavedIMREG	- $$
PageTableNumber		equ	_PageTableNumber- $$

DataLen			equ	$ - LABEL_DATA
; END of [SECTION .data1]


; IDT
[SECTION .idt]
ALIGN	32
[BITS	32]
LABEL_IDT:
; 中断门                 目标段选择子,       偏移, 			DCount, 属性
%rep 32
				Gate	SelectorCode32, SpuriousHandler,      0, DA_386IGate	;0-1Fh号中断都由32位代码段401行的_SpuriousHandler函数处理
%endrep
.020h:			Gate	SelectorCode32,    ClockHandler,      0, DA_386IGate	;20h号中断由32位代码段387行的_ClockHandler函数处理
%rep 95
				Gate	SelectorCode32, SpuriousHandler,      0, DA_386IGate	;21h-7Fh号中断都由32位代码段401行的_SpuriousHandler函数处理
%endrep
.080h:			Gate	SelectorCode32,  UserIntHandler,      0, DA_386IGate	;80h号中断由32位代码段394行的_UserIntHandler函数处理

IdtLen		equ	$ - LABEL_IDT	;IDT表的长度(当前地址-LABEL_IDT处地址)
IdtPtr		dw	IdtLen		; 段界限
		dd	0		; 基地址(在216行会被改变)		;IdtPtr是关于IDT的一个小的数据结构
; END of [SECTION .idt]


; 全局堆栈段
[SECTION .gs]
ALIGN	32
[BITS	32]
LABEL_STACK:
	times 512 db 0

TopOfStack	equ	$ - LABEL_STACK

; END of [SECTION .gs]


[SECTION .s16]
[BITS	16]
LABEL_BEGIN:
	mov	ax, cs
	mov	ds, ax
	mov	es, ax
	mov	ss, ax
	mov	sp, 0100h

	mov	[LABEL_GO_BACK_TO_REAL+3], ax
	mov	[_wSPValueInRealMode], sp

	; 得到内存数
	mov	ebx, 0
	mov	di, _MemChkBuf
.loop:
	mov	eax, 0E820h
	mov	ecx, 20
	mov	edx, 0534D4150h
	int	15h
	jc	LABEL_MEM_CHK_FAIL
	add	di, 20
	inc	dword [_dwMCRNumber]
	cmp	ebx, 0
	jne	.loop
	jmp	LABEL_MEM_CHK_OK
LABEL_MEM_CHK_FAIL:
	mov	dword [_dwMCRNumber], 0
LABEL_MEM_CHK_OK:

	; 初始化 16 位代码段描述符
	mov	ax, cs
	movzx	eax, ax
	shl	eax, 4
	add	eax, LABEL_SEG_CODE16
	mov	word [LABEL_DESC_CODE16 + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_CODE16 + 4], al
	mov	byte [LABEL_DESC_CODE16 + 7], ah

	; 初始化 32 位代码段描述符
	xor	eax, eax
	mov	ax, cs
	shl	eax, 4
	add	eax, LABEL_SEG_CODE32
	mov	word [LABEL_DESC_CODE32 + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_CODE32 + 4], al
	mov	byte [LABEL_DESC_CODE32 + 7], ah

	; 初始化数据段描述符
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_DATA
	mov	word [LABEL_DESC_DATA + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_DATA + 4], al
	mov	byte [LABEL_DESC_DATA + 7], ah

	; 初始化堆栈段描述符
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_STACK
	mov	word [LABEL_DESC_STACK + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_STACK + 4], al
	mov	byte [LABEL_DESC_STACK + 7], ah

	; 为加载 GDTR 作准备
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_GDT		; eax <- gdt 基地址
	mov	dword [GdtPtr + 2], eax	; [GdtPtr + 2] <- gdt 基地址

	; 为加载 IDTR 作准备
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_IDT		; eax <- idt 基地址
	mov	dword [IdtPtr + 2], eax	; [IdtPtr + 2] <- idt 基地址

	; 保存 IDTR
	sidt	[_SavedIDTR]	;_SavedIDTR定义于73行，用于保存IDTR

	; 保存中断屏蔽寄存器(IMREG)值
	in	al, 21h		;从21h端口获取中断屏蔽寄存器(IMREG)值
	mov	[_SavedIMREG], al	;_SavedIMREG定义于75行，用于保存中断屏蔽寄存器(IMREG)值

	; 加载 GDTR
	lgdt	[GdtPtr]

	; 关中断
	;cli

	; 加载 IDTR
	lidt	[IdtPtr]

	; 打开地址线A20
	in	al, 92h
	or	al, 00000010b
	out	92h, al

	; 准备切换到保护模式
	mov	eax, cr0
	or	eax, 1
	mov	cr0, eax

	; 真正进入保护模式
	jmp	dword SelectorCode32:0	; 执行这一句会把 SelectorCode32 装入 cs, 并跳转到 Code32Selector:0  处

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LABEL_REAL_ENTRY:		; 从保护模式跳回到实模式就到了这里
	mov	ax, cs
	mov	ds, ax
	mov	es, ax
	mov	ss, ax
	mov	sp, [_wSPValueInRealMode]

	lidt	[_SavedIDTR]	;从219行保存的_SavedIDTR变量中恢复 IDTR 的原值

	mov	al, [_SavedIMREG]	
	out	21h, al			;从223行保存的_SavedIMREG变量中恢复中断屏蔽寄存器(IMREG)的原值

	in	al, 92h		; ┓
	and	al, 11111101b	; ┣ 关闭 A20 地址线
	out	92h, al		; ┛

	sti			; 开中断

	mov	ax, 4c00h	; ┓
	int	21h		; ┛回到 DOS
; END of [SECTION .s16]


[SECTION .s32]; 32 位代码段. 由实模式跳入.
[BITS	32]

LABEL_SEG_CODE32:
	mov	ax, SelectorData
	mov	ds, ax			; 数据段选择子
	mov	es, ax
	mov	ax, SelectorVideo
	mov	gs, ax			; 视频段(屏幕缓冲区)选择子

	mov	ax, SelectorStack
	mov	ss, ax			; 堆栈段选择子
	mov	esp, TopOfStack

	call	Init8259A		;调用311行Init8259A,初始化8259A，写入ICW1,2,3,4和OCW1，仅接受时钟中断

	int	080h	;产生80号中断，右上角出现字母'I'
	sti		;开中断,设置IF位(虽然实模式下并没有关闭中断，但为了确保IF已经被设置，还是加上此句比较好)
	jmp	$	;陷入死循环;定时器定时产生时钟中断，由319行代码知，时钟中断的中断号是20h，查IDT知_ClockHandler处理时钟中断，该函数将上述字母对应值递增以在右上角循环显示对应ASCII码值
	;该函数中此后代码均不执行
	; 下面显示一个字符串
	push	szPMMessage		;将_szPMMessage地址入栈,szPMMessage在数据段中定义
	call	DispStr		;显示"In Protect Mode now. ^-^"
	add	esp, 4			;将栈指针下移，不使用pop就将字符串地址占用的空间释放

	push	szMemChkTitle		;将_szMemChkTitle地址入栈,szMemChkTitle在数据段中定义
	call	DispStr		;显示"BaseAddrL BaseAddrH LengthLow LengthHigh   Type"
	add	esp, 4			;将栈指针下移，不使用pop就将字符串地址占用的空间释放

	call	DispMemSize		; 显示内存信息

	call	PagingDemo		; 演示改变页目录的效果

	call	SetRealmode8259A	; 将8259A设置为实模式的状态

	; 到此停止
	jmp	SelectorCode16:0

; Init8259A ---------------------------------------------------------------------------------------------
Init8259A:
	mov	al, 011h	;第0位置1,表示需要ICW4,第4位置1,因为该位对ICW1必须为1
	out	020h, al	;往端口20(主8259)写入ICW1.
	call	io_delay	;四条空指令,为延迟函数，以等待操作完成,在379行定义

	out	0A0h, al	;往端口A0(从8259)写入ICW1.意义同上
	call	io_delay

	mov	al, 020h	;第5位置1，表示IRQ0对应中断向量号0x20
	out	021h, al	;往端口21(主8259)写入ICW2.
	call	io_delay

	mov	al, 028h	;第3、5位置1，表示IRQ8对应中断向量号0x28
	out	0A1h, al	;往端口A1(从8259)写入ICW2.
	call	io_delay

	mov	al, 004h	;第2位置1，表示IR2连接一块从8259
	out	021h, al	;往端口21(主8259)写入ICW3.
	call	io_delay

	mov	al, 002h	;第1位置1，表示该从8259连接在主8259的的IR2上
	out	0A1h, al	;往端口A1(从8259)写入ICW3.
	call	io_delay

	mov	al, 001h	;第0位置1，表示80x86模式
	out	021h, al	;往端口21(主8259)写入ICW4.
	call	io_delay

	out	0A1h, al	;往端口A1(从8259)写入ICW4.意义同上.
	call	io_delay

	mov	al, 11111110b	;第0位置0,仅仅开启时钟中断(IRQ0)
	;mov	al, 11111111b	; 屏蔽主8259所有中断
	out	021h, al	;往端口21(主8259)写入OCW1.
	call	io_delay

	mov	al, 11111111b	;所有位置1,屏蔽从8259所有中断
	out	0A1h, al	;往端口A1(从8259)写入OCW1.
	call	io_delay

	ret
; Init8259A ---------------------------------------------------------------------------------------------


; 将8259A设置为实模式的状态 ---------------------------------------------------------------------------------------------
SetRealmode8259A:
	mov	ax, SelectorData
	mov	fs, ax		;数据段选择子

	mov	al, 017h	;第4，2，1，0位置1，由级联8259变为单个8259；由8字节中断向量变为4字节中断向量
	out	020h, al	;往端口20(主8259)写入ICW1.
	call	io_delay

	mov	al, 008h	;IRQ0 对应中断向量 0x8
	out	021h, al	;往端口21(主8259)写入ICW2.
	call	io_delay
					;因为此时位单个8259，所以ICW3无需赋值，它不会被用到
	mov	al, 001h	;第0位置1，表示80x86模式
	out	021h, al	;往端口21(主8259)写入ICW4.
	call	io_delay

	mov	al, [fs:SavedIMREG]	
	out	021h, al		;从223行保存的_SavedIMREG变量中恢复中断屏蔽寄存器(IMREG)的原值
	call	io_delay

	ret
; SetRealmode8259A ---------------------------------------------------------------------------------------------

io_delay:	;四个连续的空语句，用于等待某操作完成
	nop
	nop
	nop
	nop
	ret

; 定义中断处理函数 ---------------------------------------------------------------
_ClockHandler:			;通过时钟触发，将右上角的字母对应的值+1并显示值对应的ASCII字符
ClockHandler	equ	_ClockHandler - $$		;_ClockHandler相对于本节开始处的偏移
	inc	byte [gs:((80 * 0 + 70) * 2)]	; 使屏幕第0行,第70列的值+1，显示对应的ASCII字符
	mov	al, 20h
	out	20h, al				; 通过发送第5位置1的OCW2，向8259A发送EOI，以便继续接收中断(本例中为时钟中断)
	iretd

_UserIntHandler:		;通过INT 80h引发，在屏幕右上角显示'I'
UserIntHandler	equ	_UserIntHandler - $$	;_UserIntHandler相对于本节开始处的偏移
	mov	ah, 0Ch				; 0000: 黑底    1100: 红字
	mov	al, 'I'
	mov	[gs:((80 * 0 + 70) * 2)], ax	;在屏幕第0行,第70列显示字母'I'
	iretd

_SpuriousHandler:		;此程序中没有出现该中断处理程序能够处理的中断
SpuriousHandler	equ	_SpuriousHandler - $$	;_SpuriousHandler相对于本节开始处的偏移
	mov	ah, 0Ch				; 0000: 黑底    1100: 红字
	mov	al, '!'
	mov	[gs:((80 * 0 + 75) * 2)], ax	; 在屏幕第0行第75列显示感叹号
	jmp	$	;陷入死循环
	iretd
; ---------------------------------------------------------------------------

; 启动分页机制 --------------------------------------------------------------
SetupPaging:
	; 根据内存大小计算应初始化多少PDE以及多少页表
	xor	edx, edx
	mov	eax, [dwMemSize]
	mov	ebx, 400000h	; 400000h = 4M = 4096 * 1024, 一个页表对应的内存大小
	div	ebx
	mov	ecx, eax	; 此时 ecx 为页表的个数，也即 PDE 应该的个数
	test	edx, edx
	jz	.no_remainder
	inc	ecx		; 如果余数不为 0 就需增加一个页表
.no_remainder:
	mov	[PageTableNumber], ecx	; 暂存页表个数

	; 为简化处理, 所有线性地址对应相等的物理地址. 并且不考虑内存空洞.

	; 首先初始化页目录
	mov	ax, SelectorFlatRW
	mov	es, ax
	mov	edi, PageDirBase0	; 此段首地址为 PageDirBase
	xor	eax, eax
	mov	eax, PageTblBase0 | PG_P  | PG_USU | PG_RWW
.1:
	stosd
	add	eax, 4096		; 为了简化, 所有页表在内存中是连续的.
	loop	.1

	; 再初始化所有页表
	mov	eax, [PageTableNumber]	; 页表个数
	mov	ebx, 1024		; 每个页表 1024 个 PTE
	mul	ebx
	mov	ecx, eax		; PTE个数 = 页表个数 * 1024
	mov	edi, PageTblBase0	; 此段首地址为 PageTblBase
	xor	eax, eax
	mov	eax, PG_P  | PG_USU | PG_RWW
.2:
	stosd
	add	eax, 4096		; 每一页指向 4K 的空间
	loop	.2

	mov	eax, PageDirBase0
	mov	cr3, eax
	mov	eax, cr0
	or	eax, 80000000h
	mov	cr0, eax
	jmp	short .3
.3:
	nop

	ret
; 分页机制启动完毕 ----------------------------------------------------------


; 测试分页机制 --------------------------------------------------------------
PagingDemo:
	mov	ax, cs
	mov	ds, ax
	mov	ax, SelectorFlatRW
	mov	es, ax

	push	LenFoo
	push	OffsetFoo
	push	ProcFoo
	call	MemCpy
	add	esp, 12

	push	LenBar
	push	OffsetBar
	push	ProcBar
	call	MemCpy
	add	esp, 12

	push	LenPagingDemoAll
	push	OffsetPagingDemoProc
	push	ProcPagingDemo
	call	MemCpy
	add	esp, 12

	mov	ax, SelectorData
	mov	ds, ax			; 数据段选择子
	mov	es, ax

	call	SetupPaging		; 启动分页

	call	SelectorFlatC:ProcPagingDemo
	call	PSwitch			; 切换页目录，改变地址映射关系
	call	SelectorFlatC:ProcPagingDemo

	ret
; ---------------------------------------------------------------------------


; 切换页表 ------------------------------------------------------------------
PSwitch:
	; 初始化页目录
	mov	ax, SelectorFlatRW
	mov	es, ax
	mov	edi, PageDirBase1	; 此段首地址为 PageDirBase
	xor	eax, eax
	mov	eax, PageTblBase1 | PG_P  | PG_USU | PG_RWW
	mov	ecx, [PageTableNumber]
.1:
	stosd
	add	eax, 4096		; 为了简化, 所有页表在内存中是连续的.
	loop	.1

	; 再初始化所有页表
	mov	eax, [PageTableNumber]	; 页表个数
	mov	ebx, 1024		; 每个页表 1024 个 PTE
	mul	ebx
	mov	ecx, eax		; PTE个数 = 页表个数 * 1024
	mov	edi, PageTblBase1	; 此段首地址为 PageTblBase
	xor	eax, eax
	mov	eax, PG_P  | PG_USU | PG_RWW
.2:
	stosd
	add	eax, 4096		; 每一页指向 4K 的空间
	loop	.2

	; 在此假设内存是大于 8M 的
	mov	eax, LinearAddrDemo
	shr	eax, 22
	mov	ebx, 4096
	mul	ebx
	mov	ecx, eax
	mov	eax, LinearAddrDemo
	shr	eax, 12
	and	eax, 03FFh	; 1111111111b (10 bits)
	mov	ebx, 4
	mul	ebx
	add	eax, ecx
	add	eax, PageTblBase1
	mov	dword [es:eax], ProcBar | PG_P | PG_USU | PG_RWW

	mov	eax, PageDirBase1
	mov	cr3, eax
	jmp	short .3
.3:
	nop

	ret
; ---------------------------------------------------------------------------


; PagingDemoProc ------------------------------------------------------------
PagingDemoProc:
OffsetPagingDemoProc	equ	PagingDemoProc - $$
	mov	eax, LinearAddrDemo
	call	eax
	retf
; ---------------------------------------------------------------------------
LenPagingDemoAll	equ	$ - PagingDemoProc
; ---------------------------------------------------------------------------


; foo -----------------------------------------------------------------------
foo:
OffsetFoo	equ	foo - $$
	mov	ah, 0Ch				; 0000: 黑底    1100: 红字
	mov	al, 'F'
	mov	[gs:((80 * 17 + 0) * 2)], ax	; 屏幕第 17 行, 第 0 列。
	mov	al, 'o'
	mov	[gs:((80 * 17 + 1) * 2)], ax	; 屏幕第 17 行, 第 1 列。
	mov	[gs:((80 * 17 + 2) * 2)], ax	; 屏幕第 17 行, 第 2 列。
	ret
LenFoo	equ	$ - foo
; ---------------------------------------------------------------------------


; bar -----------------------------------------------------------------------
bar:
OffsetBar	equ	bar - $$
	mov	ah, 0Ch				; 0000: 黑底    1100: 红字
	mov	al, 'B'
	mov	[gs:((80 * 18 + 0) * 2)], ax	; 屏幕第 18 行, 第 0 列。
	mov	al, 'a'
	mov	[gs:((80 * 18 + 1) * 2)], ax	; 屏幕第 18 行, 第 1 列。
	mov	al, 'r'
	mov	[gs:((80 * 18 + 2) * 2)], ax	; 屏幕第 18 行, 第 2 列。
	ret
LenBar	equ	$ - bar
; ---------------------------------------------------------------------------


; 显示内存信息 --------------------------------------------------------------
DispMemSize:
	push	esi
	push	edi
	push	ecx

	mov	esi, MemChkBuf
	mov	ecx, [dwMCRNumber]	;for(int i=0;i<[MCRNumber];i++) // 每次得到一个ARDS(Address Range Descriptor Structure)结构
.loop:					;{
	mov	edx, 5			;	for(int j=0;j<5;j++)	// 每次得到一个ARDS中的成员，共5个成员
	mov	edi, ARDStruct		;	{			// 依次显示：BaseAddrLow，BaseAddrHigh，LengthLow，LengthHigh，Type
.1:					;
	push	dword [esi]		;
	call	DispInt			;		DispInt(MemChkBuf[j*4]); // 显示一个成员
	pop	eax			;
	stosd				;		ARDStruct[j*4] = MemChkBuf[j*4];
	add	esi, 4			;
	dec	edx			;
	cmp	edx, 0			;
	jnz	.1			;	}
	call	DispReturn		;	printf("\n");
	cmp	dword [dwType], 1	;	if(Type == AddressRangeMemory) // AddressRangeMemory : 1, AddressRangeReserved : 2
	jne	.2			;	{
	mov	eax, [dwBaseAddrLow]	;
	add	eax, [dwLengthLow]	;
	cmp	eax, [dwMemSize]	;		if(BaseAddrLow + LengthLow > MemSize)
	jb	.2			;
	mov	[dwMemSize], eax	;			MemSize = BaseAddrLow + LengthLow;
.2:					;	}
	loop	.loop			;}
					;
	call	DispReturn		;printf("\n");
	push	szRAMSize		;
	call	DispStr			;printf("RAM size:");
	add	esp, 4			;
					;
	push	dword [dwMemSize]	;
	call	DispInt			;DispInt(MemSize);
	add	esp, 4			;

	pop	ecx
	pop	edi
	pop	esi
	ret
; ---------------------------------------------------------------------------

%include	"lib.inc"	; 库函数

SegCode32Len	equ	$ - LABEL_SEG_CODE32
; END of [SECTION .s32]


; 16 位代码段. 由 32 位代码段跳入, 跳出后到实模式
[SECTION .s16code]
ALIGN	32
[BITS	16]
LABEL_SEG_CODE16:
	; 跳回实模式:
	mov	ax, SelectorNormal
	mov	ds, ax
	mov	es, ax
	mov	fs, ax
	mov	gs, ax
	mov	ss, ax

	mov	eax, cr0
	and	al, 11111110b
	mov	cr0, eax

LABEL_GO_BACK_TO_REAL:
	jmp	0:LABEL_REAL_ENTRY	; 段地址会在程序开始处被设置成正确的值

Code16Len	equ	$ - LABEL_SEG_CODE16

; END of [SECTION .s16code]
