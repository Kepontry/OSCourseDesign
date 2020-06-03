; ==========================================
; pmtest9.asm
; ���뷽����nasm pmtest9.asm -o pmtest9.com
; ==========================================

%include	"pm.inc"	; ����, ��, �Լ�һЩ˵��

PageDirBase0		equ	200000h	; ҳĿ¼��ʼ��ַ:	2M
PageTblBase0		equ	201000h	; ҳ��ʼ��ַ:		2M +  4K
PageDirBase1		equ	210000h	; ҳĿ¼��ʼ��ַ:	2M + 64K
PageTblBase1		equ	211000h	; ҳ��ʼ��ַ:		2M + 64K + 4K

LinearAddrDemo	equ	00401000h
ProcFoo		equ	00401000h
ProcBar		equ	00501000h

ProcPagingDemo	equ	00301000h

org	0100h
	jmp	LABEL_BEGIN

[SECTION .gdt]
; GDT
;                                         �λ�ַ,       �ν���     , ����
LABEL_GDT:		Descriptor	       0,                 0, 0				; ��������
LABEL_DESC_NORMAL:	Descriptor	       0,            0ffffh, DA_DRW			; Normal ������
LABEL_DESC_FLAT_C:	Descriptor             0,           0fffffh, DA_CR | DA_32 | DA_LIMIT_4K; 0 ~ 4G
LABEL_DESC_FLAT_RW:	Descriptor             0,           0fffffh, DA_DRW | DA_LIMIT_4K	; 0 ~ 4G
LABEL_DESC_CODE32:	Descriptor	       0,  SegCode32Len - 1, DA_CR | DA_32		; ��һ�´����, 32
LABEL_DESC_CODE16:	Descriptor	       0,            0ffffh, DA_C			; ��һ�´����, 16
LABEL_DESC_DATA:	Descriptor	       0,	DataLen - 1, DA_DRW			; Data
LABEL_DESC_STACK:	Descriptor	       0,        TopOfStack, DA_DRWA | DA_32		; Stack, 32 λ
LABEL_DESC_VIDEO:	Descriptor	 0B8000h,            0ffffh, DA_DRW			; �Դ��׵�ַ
; GDT ����

GdtLen		equ	$ - LABEL_GDT	; GDT����
GdtPtr		dw	GdtLen		; GDT����
		dd	0		; GDT����ַ

; GDT ѡ����
SelectorNormal		equ	LABEL_DESC_NORMAL	- LABEL_GDT
SelectorFlatC		equ	LABEL_DESC_FLAT_C	- LABEL_GDT
SelectorFlatRW		equ	LABEL_DESC_FLAT_RW	- LABEL_GDT
SelectorCode32		equ	LABEL_DESC_CODE32	- LABEL_GDT
SelectorCode16		equ	LABEL_DESC_CODE16	- LABEL_GDT
SelectorData		equ	LABEL_DESC_DATA		- LABEL_GDT
SelectorStack		equ	LABEL_DESC_STACK	- LABEL_GDT
SelectorVideo		equ	LABEL_DESC_VIDEO	- LABEL_GDT
; END of [SECTION .gdt]

[SECTION .data1]	 ; ���ݶ�
ALIGN	32
[BITS	32]
LABEL_DATA:
; ʵģʽ��ʹ����Щ����
; �ַ���
_szPMMessage:			db	"In Protect Mode now. ^-^", 0Ah, 0Ah, 0	; ���뱣��ģʽ����ʾ���ַ���,0A������,0�����ַ�����β'\0',��Щ������DispStr�л�õ�����
_szMemChkTitle:			db	"BaseAddrL BaseAddrH LengthLow LengthHigh   Type", 0Ah, 0	; ���뱣��ģʽ����ʾ���ַ���,0A������,0�����ַ�����β'\0'
_szRAMSize			db	"RAM size:", 0		;��DispMemSize(��ʾ�ڴ���Ϣ)�б�ʹ��,0�����ַ�����β'\0'
_szReturn			db	0Ah, 0	;�൱��һ�����з�����DispReturn�б�ʹ��
; ����
_wSPValueInRealMode		dw	0
_dwMCRNumber:			dd	0	; Memory Check Result
_dwDispPos:			dd	(80 * 6 + 0) * 2	; ��Ļ�� 6 ��, �� 0 �С�������ʾ��һ����������Ļ�Ͻ�Ҫ�����λ��
_dwMemSize:			dd	0
_ARDStruct:			; Address Range Descriptor Structure
	_dwBaseAddrLow:		dd	0
	_dwBaseAddrHigh:	dd	0
	_dwLengthLow:		dd	0
	_dwLengthHigh:		dd	0
	_dwType:		dd	0
_PageTableNumber:		dd	0
_SavedIDTR:			dd	0	;���ڱ��� IDTR
				dd	0
_SavedIMREG:			db	0	;���ڱ����ж����μĴ���ֵ
_MemChkBuf:	times	256	db	0

; ����ģʽ��ʹ����Щ����
szPMMessage		equ	_szPMMessage	- $$
szMemChkTitle		equ	_szMemChkTitle	- $$
szRAMSize		equ	_szRAMSize	- $$
szReturn		equ	_szReturn	- $$
dwDispPos		equ	_dwDispPos	- $$	;_dwDispPos��������ڱ��ڿ�ʼ����ƫ��
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
; �ж���                 Ŀ���ѡ����,       ƫ��, 			DCount, ����
%rep 32
				Gate	SelectorCode32, SpuriousHandler,      0, DA_386IGate	;0-1Fh���ж϶���32λ�����401�е�_SpuriousHandler��������
%endrep
.020h:			Gate	SelectorCode32,    ClockHandler,      0, DA_386IGate	;20h���ж���32λ�����387�е�_ClockHandler��������
%rep 95
				Gate	SelectorCode32, SpuriousHandler,      0, DA_386IGate	;21h-7Fh���ж϶���32λ�����401�е�_SpuriousHandler��������
%endrep
.080h:			Gate	SelectorCode32,  UserIntHandler,      0, DA_386IGate	;80h���ж���32λ�����394�е�_UserIntHandler��������

IdtLen		equ	$ - LABEL_IDT	;IDT��ĳ���(��ǰ��ַ-LABEL_IDT����ַ)
IdtPtr		dw	IdtLen		; �ν���
		dd	0		; ����ַ(��216�лᱻ�ı�)		;IdtPtr�ǹ���IDT��һ��С�����ݽṹ
; END of [SECTION .idt]


; ȫ�ֶ�ջ��
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

	; �õ��ڴ���
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

	; ��ʼ�� 16 λ�����������
	mov	ax, cs
	movzx	eax, ax
	shl	eax, 4
	add	eax, LABEL_SEG_CODE16
	mov	word [LABEL_DESC_CODE16 + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_CODE16 + 4], al
	mov	byte [LABEL_DESC_CODE16 + 7], ah

	; ��ʼ�� 32 λ�����������
	xor	eax, eax
	mov	ax, cs
	shl	eax, 4
	add	eax, LABEL_SEG_CODE32
	mov	word [LABEL_DESC_CODE32 + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_CODE32 + 4], al
	mov	byte [LABEL_DESC_CODE32 + 7], ah

	; ��ʼ�����ݶ�������
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_DATA
	mov	word [LABEL_DESC_DATA + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_DATA + 4], al
	mov	byte [LABEL_DESC_DATA + 7], ah

	; ��ʼ����ջ��������
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_STACK
	mov	word [LABEL_DESC_STACK + 2], ax
	shr	eax, 16
	mov	byte [LABEL_DESC_STACK + 4], al
	mov	byte [LABEL_DESC_STACK + 7], ah

	; Ϊ���� GDTR ��׼��
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_GDT		; eax <- gdt ����ַ
	mov	dword [GdtPtr + 2], eax	; [GdtPtr + 2] <- gdt ����ַ

	; Ϊ���� IDTR ��׼��
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_IDT		; eax <- idt ����ַ
	mov	dword [IdtPtr + 2], eax	; [IdtPtr + 2] <- idt ����ַ

	; ���� IDTR
	sidt	[_SavedIDTR]	;_SavedIDTR������73�У����ڱ���IDTR

	; �����ж����μĴ���(IMREG)ֵ
	in	al, 21h		;��21h�˿ڻ�ȡ�ж����μĴ���(IMREG)ֵ
	mov	[_SavedIMREG], al	;_SavedIMREG������75�У����ڱ����ж����μĴ���(IMREG)ֵ

	; ���� GDTR
	lgdt	[GdtPtr]

	; ���ж�
	;cli

	; ���� IDTR
	lidt	[IdtPtr]

	; �򿪵�ַ��A20
	in	al, 92h
	or	al, 00000010b
	out	92h, al

	; ׼���л�������ģʽ
	mov	eax, cr0
	or	eax, 1
	mov	cr0, eax

	; �������뱣��ģʽ
	jmp	dword SelectorCode32:0	; ִ����һ���� SelectorCode32 װ�� cs, ����ת�� Code32Selector:0  ��

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LABEL_REAL_ENTRY:		; �ӱ���ģʽ���ص�ʵģʽ�͵�������
	mov	ax, cs
	mov	ds, ax
	mov	es, ax
	mov	ss, ax
	mov	sp, [_wSPValueInRealMode]

	lidt	[_SavedIDTR]	;��219�б����_SavedIDTR�����лָ� IDTR ��ԭֵ

	mov	al, [_SavedIMREG]	
	out	21h, al			;��223�б����_SavedIMREG�����лָ��ж����μĴ���(IMREG)��ԭֵ

	in	al, 92h		; ��
	and	al, 11111101b	; �� �ر� A20 ��ַ��
	out	92h, al		; ��

	sti			; ���ж�

	mov	ax, 4c00h	; ��
	int	21h		; ���ص� DOS
; END of [SECTION .s16]


[SECTION .s32]; 32 λ�����. ��ʵģʽ����.
[BITS	32]

LABEL_SEG_CODE32:
	mov	ax, SelectorData
	mov	ds, ax			; ���ݶ�ѡ����
	mov	es, ax
	mov	ax, SelectorVideo
	mov	gs, ax			; ��Ƶ��(��Ļ������)ѡ����

	mov	ax, SelectorStack
	mov	ss, ax			; ��ջ��ѡ����
	mov	esp, TopOfStack

	call	Init8259A		;����311��Init8259A,��ʼ��8259A��д��ICW1,2,3,4��OCW1��������ʱ���ж�

	int	080h	;����80���жϣ����Ͻǳ�����ĸ'I'
	sti		;���ж�,����IFλ(��Ȼʵģʽ�²�û�йر��жϣ���Ϊ��ȷ��IF�Ѿ������ã����Ǽ��ϴ˾�ȽϺ�)
	jmp	$	;������ѭ��;��ʱ����ʱ����ʱ���жϣ���319�д���֪��ʱ���жϵ��жϺ���20h����IDT֪_ClockHandler����ʱ���жϣ��ú�����������ĸ��Ӧֵ�����������Ͻ�ѭ����ʾ��ӦASCII��ֵ
	;�ú����д˺�������ִ��
	; ������ʾһ���ַ���
	push	szPMMessage		;��_szPMMessage��ַ��ջ,szPMMessage�����ݶ��ж���
	call	DispStr		;��ʾ"In Protect Mode now. ^-^"
	add	esp, 4			;��ջָ�����ƣ���ʹ��pop�ͽ��ַ�����ַռ�õĿռ��ͷ�

	push	szMemChkTitle		;��_szMemChkTitle��ַ��ջ,szMemChkTitle�����ݶ��ж���
	call	DispStr		;��ʾ"BaseAddrL BaseAddrH LengthLow LengthHigh   Type"
	add	esp, 4			;��ջָ�����ƣ���ʹ��pop�ͽ��ַ�����ַռ�õĿռ��ͷ�

	call	DispMemSize		; ��ʾ�ڴ���Ϣ

	call	PagingDemo		; ��ʾ�ı�ҳĿ¼��Ч��

	call	SetRealmode8259A	; ��8259A����Ϊʵģʽ��״̬

	; ����ֹͣ
	jmp	SelectorCode16:0

; Init8259A ---------------------------------------------------------------------------------------------
Init8259A:
	mov	al, 011h	;��0λ��1,��ʾ��ҪICW4,��4λ��1,��Ϊ��λ��ICW1����Ϊ1
	out	020h, al	;���˿�20(��8259)д��ICW1.
	call	io_delay	;������ָ��,Ϊ�ӳٺ������Եȴ��������,��379�ж���

	out	0A0h, al	;���˿�A0(��8259)д��ICW1.����ͬ��
	call	io_delay

	mov	al, 020h	;��5λ��1����ʾIRQ0��Ӧ�ж�������0x20
	out	021h, al	;���˿�21(��8259)д��ICW2.
	call	io_delay

	mov	al, 028h	;��3��5λ��1����ʾIRQ8��Ӧ�ж�������0x28
	out	0A1h, al	;���˿�A1(��8259)д��ICW2.
	call	io_delay

	mov	al, 004h	;��2λ��1����ʾIR2����һ���8259
	out	021h, al	;���˿�21(��8259)д��ICW3.
	call	io_delay

	mov	al, 002h	;��1λ��1����ʾ�ô�8259��������8259�ĵ�IR2��
	out	0A1h, al	;���˿�A1(��8259)д��ICW3.
	call	io_delay

	mov	al, 001h	;��0λ��1����ʾ80x86ģʽ
	out	021h, al	;���˿�21(��8259)д��ICW4.
	call	io_delay

	out	0A1h, al	;���˿�A1(��8259)д��ICW4.����ͬ��.
	call	io_delay

	mov	al, 11111110b	;��0λ��0,��������ʱ���ж�(IRQ0)
	;mov	al, 11111111b	; ������8259�����ж�
	out	021h, al	;���˿�21(��8259)д��OCW1.
	call	io_delay

	mov	al, 11111111b	;����λ��1,���δ�8259�����ж�
	out	0A1h, al	;���˿�A1(��8259)д��OCW1.
	call	io_delay

	ret
; Init8259A ---------------------------------------------------------------------------------------------


; ��8259A����Ϊʵģʽ��״̬ ---------------------------------------------------------------------------------------------
SetRealmode8259A:
	mov	ax, SelectorData
	mov	fs, ax		;���ݶ�ѡ����

	mov	al, 017h	;��4��2��1��0λ��1���ɼ���8259��Ϊ����8259����8�ֽ��ж�������Ϊ4�ֽ��ж�����
	out	020h, al	;���˿�20(��8259)д��ICW1.
	call	io_delay

	mov	al, 008h	;IRQ0 ��Ӧ�ж����� 0x8
	out	021h, al	;���˿�21(��8259)д��ICW2.
	call	io_delay
					;��Ϊ��ʱλ����8259������ICW3���踳ֵ�������ᱻ�õ�
	mov	al, 001h	;��0λ��1����ʾ80x86ģʽ
	out	021h, al	;���˿�21(��8259)д��ICW4.
	call	io_delay

	mov	al, [fs:SavedIMREG]	
	out	021h, al		;��223�б����_SavedIMREG�����лָ��ж����μĴ���(IMREG)��ԭֵ
	call	io_delay

	ret
; SetRealmode8259A ---------------------------------------------------------------------------------------------

io_delay:	;�ĸ������Ŀ���䣬���ڵȴ�ĳ�������
	nop
	nop
	nop
	nop
	ret

; �����жϴ����� ---------------------------------------------------------------
_ClockHandler:			;ͨ��ʱ�Ӵ����������Ͻǵ���ĸ��Ӧ��ֵ+1����ʾֵ��Ӧ��ASCII�ַ�
ClockHandler	equ	_ClockHandler - $$		;_ClockHandler����ڱ��ڿ�ʼ����ƫ��
	inc	byte [gs:((80 * 0 + 70) * 2)]	; ʹ��Ļ��0��,��70�е�ֵ+1����ʾ��Ӧ��ASCII�ַ�
	mov	al, 20h
	out	20h, al				; ͨ�����͵�5λ��1��OCW2����8259A����EOI���Ա���������ж�(������Ϊʱ���ж�)
	iretd

_UserIntHandler:		;ͨ��INT 80h����������Ļ���Ͻ���ʾ'I'
UserIntHandler	equ	_UserIntHandler - $$	;_UserIntHandler����ڱ��ڿ�ʼ����ƫ��
	mov	ah, 0Ch				; 0000: �ڵ�    1100: ����
	mov	al, 'I'
	mov	[gs:((80 * 0 + 70) * 2)], ax	;����Ļ��0��,��70����ʾ��ĸ'I'
	iretd

_SpuriousHandler:		;�˳�����û�г��ָ��жϴ�������ܹ�������ж�
SpuriousHandler	equ	_SpuriousHandler - $$	;_SpuriousHandler����ڱ��ڿ�ʼ����ƫ��
	mov	ah, 0Ch				; 0000: �ڵ�    1100: ����
	mov	al, '!'
	mov	[gs:((80 * 0 + 75) * 2)], ax	; ����Ļ��0�е�75����ʾ��̾��
	jmp	$	;������ѭ��
	iretd
; ---------------------------------------------------------------------------

; ������ҳ���� --------------------------------------------------------------
SetupPaging:
	; �����ڴ��С����Ӧ��ʼ������PDE�Լ�����ҳ��
	xor	edx, edx
	mov	eax, [dwMemSize]
	mov	ebx, 400000h	; 400000h = 4M = 4096 * 1024, һ��ҳ���Ӧ���ڴ��С
	div	ebx
	mov	ecx, eax	; ��ʱ ecx Ϊҳ��ĸ�����Ҳ�� PDE Ӧ�õĸ���
	test	edx, edx
	jz	.no_remainder
	inc	ecx		; ���������Ϊ 0 ��������һ��ҳ��
.no_remainder:
	mov	[PageTableNumber], ecx	; �ݴ�ҳ�����

	; Ϊ�򻯴���, �������Ե�ַ��Ӧ��ȵ������ַ. ���Ҳ������ڴ�ն�.

	; ���ȳ�ʼ��ҳĿ¼
	mov	ax, SelectorFlatRW
	mov	es, ax
	mov	edi, PageDirBase0	; �˶��׵�ַΪ PageDirBase
	xor	eax, eax
	mov	eax, PageTblBase0 | PG_P  | PG_USU | PG_RWW
.1:
	stosd
	add	eax, 4096		; Ϊ�˼�, ����ҳ�����ڴ�����������.
	loop	.1

	; �ٳ�ʼ������ҳ��
	mov	eax, [PageTableNumber]	; ҳ�����
	mov	ebx, 1024		; ÿ��ҳ�� 1024 �� PTE
	mul	ebx
	mov	ecx, eax		; PTE���� = ҳ����� * 1024
	mov	edi, PageTblBase0	; �˶��׵�ַΪ PageTblBase
	xor	eax, eax
	mov	eax, PG_P  | PG_USU | PG_RWW
.2:
	stosd
	add	eax, 4096		; ÿһҳָ�� 4K �Ŀռ�
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
; ��ҳ����������� ----------------------------------------------------------


; ���Է�ҳ���� --------------------------------------------------------------
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
	mov	ds, ax			; ���ݶ�ѡ����
	mov	es, ax

	call	SetupPaging		; ������ҳ

	call	SelectorFlatC:ProcPagingDemo
	call	PSwitch			; �л�ҳĿ¼���ı��ַӳ���ϵ
	call	SelectorFlatC:ProcPagingDemo

	ret
; ---------------------------------------------------------------------------


; �л�ҳ�� ------------------------------------------------------------------
PSwitch:
	; ��ʼ��ҳĿ¼
	mov	ax, SelectorFlatRW
	mov	es, ax
	mov	edi, PageDirBase1	; �˶��׵�ַΪ PageDirBase
	xor	eax, eax
	mov	eax, PageTblBase1 | PG_P  | PG_USU | PG_RWW
	mov	ecx, [PageTableNumber]
.1:
	stosd
	add	eax, 4096		; Ϊ�˼�, ����ҳ�����ڴ�����������.
	loop	.1

	; �ٳ�ʼ������ҳ��
	mov	eax, [PageTableNumber]	; ҳ�����
	mov	ebx, 1024		; ÿ��ҳ�� 1024 �� PTE
	mul	ebx
	mov	ecx, eax		; PTE���� = ҳ����� * 1024
	mov	edi, PageTblBase1	; �˶��׵�ַΪ PageTblBase
	xor	eax, eax
	mov	eax, PG_P  | PG_USU | PG_RWW
.2:
	stosd
	add	eax, 4096		; ÿһҳָ�� 4K �Ŀռ�
	loop	.2

	; �ڴ˼����ڴ��Ǵ��� 8M ��
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
	mov	ah, 0Ch				; 0000: �ڵ�    1100: ����
	mov	al, 'F'
	mov	[gs:((80 * 17 + 0) * 2)], ax	; ��Ļ�� 17 ��, �� 0 �С�
	mov	al, 'o'
	mov	[gs:((80 * 17 + 1) * 2)], ax	; ��Ļ�� 17 ��, �� 1 �С�
	mov	[gs:((80 * 17 + 2) * 2)], ax	; ��Ļ�� 17 ��, �� 2 �С�
	ret
LenFoo	equ	$ - foo
; ---------------------------------------------------------------------------


; bar -----------------------------------------------------------------------
bar:
OffsetBar	equ	bar - $$
	mov	ah, 0Ch				; 0000: �ڵ�    1100: ����
	mov	al, 'B'
	mov	[gs:((80 * 18 + 0) * 2)], ax	; ��Ļ�� 18 ��, �� 0 �С�
	mov	al, 'a'
	mov	[gs:((80 * 18 + 1) * 2)], ax	; ��Ļ�� 18 ��, �� 1 �С�
	mov	al, 'r'
	mov	[gs:((80 * 18 + 2) * 2)], ax	; ��Ļ�� 18 ��, �� 2 �С�
	ret
LenBar	equ	$ - bar
; ---------------------------------------------------------------------------


; ��ʾ�ڴ���Ϣ --------------------------------------------------------------
DispMemSize:
	push	esi
	push	edi
	push	ecx

	mov	esi, MemChkBuf
	mov	ecx, [dwMCRNumber]	;for(int i=0;i<[MCRNumber];i++) // ÿ�εõ�һ��ARDS(Address Range Descriptor Structure)�ṹ
.loop:					;{
	mov	edx, 5			;	for(int j=0;j<5;j++)	// ÿ�εõ�һ��ARDS�еĳ�Ա����5����Ա
	mov	edi, ARDStruct		;	{			// ������ʾ��BaseAddrLow��BaseAddrHigh��LengthLow��LengthHigh��Type
.1:					;
	push	dword [esi]		;
	call	DispInt			;		DispInt(MemChkBuf[j*4]); // ��ʾһ����Ա
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

%include	"lib.inc"	; �⺯��

SegCode32Len	equ	$ - LABEL_SEG_CODE32
; END of [SECTION .s32]


; 16 λ�����. �� 32 λ���������, ������ʵģʽ
[SECTION .s16code]
ALIGN	32
[BITS	16]
LABEL_SEG_CODE16:
	; ����ʵģʽ:
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
	jmp	0:LABEL_REAL_ENTRY	; �ε�ַ���ڳ���ʼ�������ó���ȷ��ֵ

Code16Len	equ	$ - LABEL_SEG_CODE16

; END of [SECTION .s16code]
