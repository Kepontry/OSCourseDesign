
; ------------------------------------------------------------------------
; 显示 AL 中的数字
; ------------------------------------------------------------------------
DispAL:		;AL中存放着待显示数字
	push	ecx
	push	edx
	push	edi		;暂存ecx,edx,edi

	mov	edi, [dwDispPos]	;将显示地址赋值给edi。dwDispPos定义于pmtest9.asm的83行中，表示_dwDispPos的偏移。

	mov	ah, 0Fh			; 0000b: 黑底    1111b: 白字
	mov	dl, al
	shr	al, 4		;dl的低4位存放数字低4位，al的低4位存放数字高4位
	mov	ecx, 2		;循环两次，将数字用两位16进制数表示
.begin:
	and	al, 01111b		;高位清0
	cmp	al, 9		;与9比较，将16进制数转换为对应的字符
	ja	.1		;大于9跳转至22行
	add	al, '0'		;小于或等于9则将0-9的数字加上'0'
	jmp	.2
.1:
	sub	al, 0Ah		
	add	al, 'A'		;将10-15的数字减10后加'A'
.2:		;将16进制数转换为对应的字符后都进入此步
	mov	[gs:edi], ax		;送入屏幕对应位置显示
	add	edi, 2		;更新edi为下一个写入位置

	mov	al, dl		;将dl中的内容赋给al，使al低4位为数字的低4位
	loop	.begin		;第一轮循环显示的是高4位，第二轮循环显示低4位
	;add	edi, 2

	mov	[dwDispPos], edi		;更新_dwDispPos，保证下次使用时显示位置正确

	pop	edi
	pop	edx
	pop	ecx		;恢复ecx,edx,edi的值

	ret		;返回
; DispAL 结束-------------------------------------------------------------


; ------------------------------------------------------------------------
; 显示一个整形数
; ------------------------------------------------------------------------
DispInt:		;进入时，栈顶往下依次是eip(call短跳转指令使eip入栈)和要显示的整形数(提前压栈)
	mov	eax, [esp + 4]		;跳过eip,取出要显示的整型数
	shr	eax, 24			;使高8位在AL中
	call	DispAL		;显示高8位

	mov	eax, [esp + 4]		;跳过eip,取出要显示的整型数
	shr	eax, 16			;使接下来的8位在AL中
	call	DispAL		;显示次高8位

	mov	eax, [esp + 4]		;跳过eip,取出要显示的整型数
	shr	eax, 8			;使AH中的值在AL中
	call	DispAL		;显示次低8位

	mov	eax, [esp + 4]		;跳过eip,取出要显示的整型数
	call	DispAL		;显示低8位

	mov	ah, 07h			; 0000b: 黑底    0111b: 灰字
	mov	al, 'h'
	push	edi		;暂存edi
	mov	edi, [dwDispPos]	;edi为显示位置的偏移
	mov	[gs:edi], ax		;显示字符'h'
	add	edi, 4
	mov	[dwDispPos], edi	;更新_dwDispPos,同时与下一个要显示的内容保持间隔
	pop	edi		;恢复edi的值

	ret		;返回
; DispInt 结束------------------------------------------------------------

; ------------------------------------------------------------------------
; 显示一个字符串
; ------------------------------------------------------------------------
DispStr:		;进入时，栈顶往下依次是eip(call短跳转指令使eip入栈)和要显示的要显示的字符串的偏移地址(提前压栈)
	push	ebp
	mov	ebp, esp		;ebp被赋值为当前栈顶指针
	push	ebx
	push	esi
	push	edi			;暂存ebp,ebx,esi,edi的值

	mov	esi, [ebp + 8]	; 此处是+8而不是+4是因为在ebp被赋值前，ebp也入栈了，所以应该跳过ebp和eip这两个值才能取到字符串的偏移地址
	mov	edi, [dwDispPos]	;将显示地址赋值给edi
	mov	ah, 0Fh			; 0000b: 黑底    1111b: 白字
.1:
	lodsb		;从ds:esi中取一个字节的数据存在al中(esi为字符串的偏移地址;进入DispStr前，ds已被赋值为数据段选择子)
	test	al, al		;检测al是否为0
	jz	.2		;如果为0(字符串以'/0'结尾)，则跳转至.2(即显示完毕后的处理)
	cmp	al, 0Ah	; 是回车吗?
	jnz	.3		;不是回车，则跳转至.3，直接显示字符
	push	eax		;是回车，先暂存eax
	mov	eax, edi
	mov	bl, 160
	div	bl
	and	eax, 0FFh	;取显示地址/160后的整数部分(即行数)
	inc	eax			;行数+1，即换行
	mov	bl, 160		
	mul	bl			;eax中为换行后的偏移
	mov	edi, eax
	pop	eax			;恢复eax的值
	jmp	.1			;显示换行完毕，继续处理下一个字符
.3:
	mov	[gs:edi], ax	;显示字符;gs在进入DispStr前已被赋值为SelectorVideo
	add	edi, 2		;更新edi为下一个写入位置
	jmp	.1			;继续处理下一个字符

.2:			;仅当al值为0时跳转至此，表示读到了'\0'，字符串已显示完毕
	mov	[dwDispPos], edi	;更新_dwDispPos

	pop	edi
	pop	esi
	pop	ebx
	pop	ebp			;恢复ebp,ebx,esi,edi的值
	ret		;返回
; DispStr 结束------------------------------------------------------------

; ------------------------------------------------------------------------
; 换行
; ------------------------------------------------------------------------
DispReturn:
	push	szReturn		;就是一个0Ah,一个'\0',在pmtest9.asm的数据段中定义
	call	DispStr			;在DispStr中，换行符会得到处理
	add	esp, 4		;将栈指针下移，不使用pop就将字符串地址szReturn占用的空间释放

	ret		;返回
; DispReturn 结束---------------------------------------------------------


; ------------------------------------------------------------------------
; 内存拷贝，仿 memcpy
; ------------------------------------------------------------------------
; void* MemCpy(void* es:pDest, void* ds:pSrc, int iSize);
; ------------------------------------------------------------------------
MemCpy:
	push	ebp
	mov	ebp, esp		;ebp被赋值为当前栈顶指针

	push	esi
	push	edi
	push	ecx
							;[ebp]存的是eip,[ebp + 4]存的是ebp
	mov	edi, [ebp + 8]		;目的地址
	mov	esi, [ebp + 12]		;源地址
	mov	ecx, [ebp + 16]		;计数器
.1:
	cmp	ecx, 0		; 判断计数器
	jz	.2		; 计数器为零时跳出

	mov	al, [ds:esi]		
	inc	esi			
					
	mov	byte [es:edi], al	
	inc	edi			;逐字节移动,将数据从ds:esi移到es:edi中

	dec	ecx		; 计数器减一
	jmp	.1		; 循环
.2:
	mov	eax, [ebp + 8]	;将传入的目的地址赋给eax,但之后调用者似乎没有使用eax

	pop	ecx
	pop	edi
	pop	esi
	mov	esp, ebp
	pop	ebp		;还原ecx,edi,esi,esp和ebp的值

	ret			; 函数结束，返回
; MemCpy 结束-------------------------------------------------------------

