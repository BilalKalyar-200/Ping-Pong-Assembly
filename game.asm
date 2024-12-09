[org 0x100]
jmp start
intro db ' -------Welcome to the Ping Pong Game-------',0
cont1: db '     Press any key to play....',0
msg1: db '      Press ESC key to exit',0
devs: db '   This game is developed by',0
dev1: db ' [1] Bilal Mohsin',0
dev2: db ' [2] Jawad Jameel',0
player1: db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0    ;names
player2: db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
pause_msg: db '    Press P for pause/unpause',0
pp1: db 'Enter player 1 name: ',0
pp2: db 'Enter player 2 name: ',0
paddle_height: dw 4        ;height of the paddle (number of rows)
paddelll: db '|'
win_msg1: db ' is the winner',0
ball: dw 0x0A2A  
pos_player1: dw 0x0907       ;padel position for player 1
pos_player2: dw 0x0948       ;padel position for player 2
movement_str: db ' For movements use W,S,Up,Down',0
erase_char: db ' '
p1_score: dw 0
p2_score: dw 0
score_str: db 'scores: ',0
ball_row: dw 12
ball_col: dw 40
max_score: dw 5         ;game limit  
delay:
    push cx       
    push dx
    mov cx, 0xFF 
outer_loop:
    mov dx, 0xFF 
inner_loop:
    dec dx         
    jnz inner_loop
    dec cx         
    jnz outer_loop
    pop dx      
    pop cx
    ret
print_ball:
push dx
push bx
push ax
push bp
    

	mov ax, 0xB800
	mov es, ax
    mov al, [ball_row]          ; Column
    mov ah, 0                   ; Zero out upper byte
    mov bl, 80                  ; Each row has 80 columns
    mul bl                      ; AX = column * 80
    mov bx, ax                  ; Save result (row offset in BX)
    mov al, [ball_col]          ; Row
    xor ah, ah                  ; Zero out upper byte
    add bx, ax                  ; Total offset = row * 80 + column
    shl bx, 1                   ; Multiply by 2 (2 bytes per character)
    mov di, bx
		
		mov ax, [ball]
		STOSW
		
pop bp
pop ax
pop bx
pop dx
ret
reset_ball:
    call clr_ball
    mov word[ball_row],12
    mov word[ball_col],40
    ret
clr_ball:
push cx
push dx
push bx
push bp
push es
push ax
    mov cx,1
    mov dh,[ball_row]
    mov dl,[ball_col]
    mov al, 01      ;subservice
    mov ah, 0x13
    mov bl,0        ;attribute (black on black)
    mov bp,erase_char   ;loading space for clearing
    push cs
    pop es
    int 0x10
pop ax
pop es
pop bp
pop bx
pop dx
pop cx
ret
          


ball_dir: dw 0
coll_found: db 0     ;flag to check if collision with padel found(of ball)
check_left_paddle:
    push ax
    push dx
    push cx

    mov ax, [pos_player1]       
    mov dx, ax                  
    shr dx, 8                   ;now dl will have row of padel
    and ax, 0x00FF              ;ax will have collumn of padel


    cmp [ball_col], ax          ;ball column == paddle column or not???
    jne no_collision_left       ;if no --> no collision

    ;check with all rows of padel (ie height)
    mov cx, [paddle_height]
row_loop_left:
    cmp [ball_row], dx          ;compare ball with current padel row
    je paddle_collision_left          ;if equal means collision
    inc dx                      ;moves to next row of padel
    loop row_loop_left

no_collision_left:
    mov byte [coll_found], 0
    pop cx
    pop dx
    pop ax
    ret

paddle_collision_left:
    mov byte [coll_found], 1
    pop cx
    pop dx
    pop ax
    ret


check_right_paddle:
    push ax
    push dx
    push cx

    mov ax, [pos_player2]        
    mov dx, ax                  
    shr dx, 8                   ;now dl will have row of padel
    and ax, 0x00FF              ;ax will have collumn of padel

    cmp [ball_col], ax           ;ball column == paddle column or not???
    jne no_collision_right       ;if no --> no collision

    ;check with all rows of padel (ie height)
    mov cx, [paddle_height]
row_loop_right:
    cmp [ball_row], dx          ;compare ball with current padel row
    je paddle_hit_right         ;if equal means collision
    inc dx                      ;moves to next row of padel
    loop row_loop_right

no_collision_right:
    mov byte [coll_found], 0
    pop cx
    pop dx
    pop ax
    ret

paddle_hit_right:
    mov byte [coll_found], 1
    pop cx
    pop dx
    pop ax
    ret

when_up_right:
    cmp word [ball_row], 2          ;check if hitted top boundary
    jne do_nothing1                 ;skip if not 
    cmp word [ball_dir], 0          ;check if moving up-right
    jne do_nothing1                 ;Skip if not
    mov word [ball_dir], 2          ;change direction to down-right
    ret
do_nothing1:
    ret

when_up_left:
    cmp word [ball_row], 2          ;check if hitted top boundary
    jne do_nothing2                 ;skip if not
    cmp word [ball_dir], 1          ;check if moving up-left
    jne do_nothing2                 ;skip if not
    mov word [ball_dir], 3          ;change direction to down-left
    ret
do_nothing2:
    ret

when_down_right:
    cmp word [ball_row], 23         ;check if hitted bottom boundary
    jne do_nothing3                 ;skip if not
    cmp word [ball_dir], 2          ;check if moving down-right
    jne do_nothing3                 ;skip if not
    mov word [ball_dir], 0          ;change direction to up-right
    ret
do_nothing3:
    ret

when_down_left:
    cmp word [ball_row], 23         ;check if hitted bottom boundary
    jne do_nothing4                 ;skip if not
    cmp word [ball_dir], 3          ;check if moving down-left
    jne do_nothing4
    mov word [ball_dir], 1          ;change direction to up-left
    ret
do_nothing4:
    ret


;this function reflects the ball based on its direction (horizontal)
handle_paddle_collision:
    cmp word [ball_dir], 0       ;if moving up-right
    je switch_to_up_left         ;reflect to up-left
    cmp word [ball_dir], 1       ;if moving up-left
    je switch_to_up_right        ;reflect to up-right
    cmp word [ball_dir], 2       ;if moving down-right
    je switch_to_down_left       ;reflect to down-left
    cmp word [ball_dir], 3       ;if moving down-left
    je switch_to_down_right      ;reflect to down-right
    ret

switch_to_down_right:
    mov word [ball_dir], 2
    ret

switch_to_down_left:
    mov word [ball_dir], 3
    ret

switch_to_up_right:
    mov word [ball_dir], 0
    ret

switch_to_up_left:
    mov word [ball_dir], 1
    ret


;this is to check collision with top boundary
find_ball_direction:
    cmp word [ball_row], 2       ;if at top boundary
    jne check_bottom_boundary    ;if not --> bottom
    cmp word [ball_dir], 0       ;if moving up-right
    je switch_to_down_right
    cmp word [ball_dir], 1       ;if moving up-left?
    je switch_to_down_left

check_bottom_boundary:
    cmp word [ball_row], 23      ;if at bottom boundary
    jne checker_left_paddle      ;if not --> check paddles
    cmp word [ball_dir], 2       ;if moving down-right
    je switch_to_up_right
    cmp word [ball_dir], 3       ;if moving down-left
    je switch_to_up_left

checker_left_paddle:
    call check_left_paddle
    cmp byte [coll_found], 1     ;paddle collision detected?
    je handle_paddle_collision
    jmp checker_right_paddle

checker_right_paddle:
    call check_right_paddle
    cmp byte [coll_found], 1     ;padle collision detected?
    je handle_paddle_collision
    jmp check_wall_collision     ;check for wall collision

check_wall_collision:
    cmp word [ball_col], 7       ;if ball missed padle (left wala)
    jl player_2_scores           ;score for Player 2
    cmp word [ball_col], 73      ;if ball missed padle (right wala)
    jg player_1_scores           ;score for Player 1
    ret                          ;no scores
player_1_scores:
    inc word [p1_score]
    call reset_ball
    ret

player_2_scores:
    inc word [p2_score]
    call reset_ball
    ret




move_ball:
    call find_ball_direction

    ;move the ball based on its direction
    cmp word [ball_dir], 0       ;moving Up-Right
    je move_up_right
    cmp word [ball_dir], 1       ;moving Up-Left
    je move_up_left
    
    cmp word [ball_dir], 2       ;moving Down-Right
    je move_down_right
    cmp word [ball_dir], 3       ;moving Down-Left
    je move_down_left
    ret

move_up_right:
    call clr_ball
    sub word [ball_row], 1       ;move up
    add word [ball_col], 1       ;move right
    call print_ball
    ret

move_up_left:
    call clr_ball
    sub word [ball_row], 1       ;move up
    sub word [ball_col], 1       ;move left
    call print_ball
    ret

move_down_right:
    call clr_ball
    add word [ball_row], 1       ;move down
    add word [ball_col], 1       ;move right
    call print_ball
    ret

move_down_left:
    call clr_ball
    add word [ball_row], 1       ;move down
    sub word [ball_col], 1       ;move left
    call print_ball
    ret



print_score_p1: ;creating seperate for both would allow us to print any time without passing anything
                ;bcz during the game we would need to print multiple times 
push ax          
push bx
push cx
push dx
push es
push di
    mov ax,[p1_score]
    mov bx,10
    mov cx,0    ;numebr of digits
    convert:
        mov dx,0
        div bx
        add dl,0x30
        push dx
        inc cx
        cmp ax,0
        jnz convert
    mov di, 16  ;position
    mov ax,0xb800
    mov es,ax
    score_print_l:
        pop dx           ;number to print
        mov dh,10       ;attribute
        mov [es:di],dx
        add di,2
        loop score_print_l 
pop di
pop es
pop dx        
pop cx
pop bx
pop ax
ret

print_score_p2:
push ax          
push bx
push cx
push dx
push es
push di
    mov ax,[p2_score]
    mov bx,10
    mov cx,0    ;numebr of digits
    convert2:
        mov dx,0
        div bx
        add dl,0x30
        push dx
        inc cx
        cmp ax,0
        jnz convert2
    mov di, 156  ;position
    mov ax,0xb800
    mov es,ax
    score_print_2:
        pop dx           ;number to print
        mov dh,10       ;attribute
        mov [es:di],dx
        add di,2
        loop score_print_2 
pop di
pop es
pop dx        
pop cx
pop bx
pop ax
ret

clrscr:
push es
push ax
push cx
push di

mov ax, 0xb800
mov es, ax 
xor di, di      ;top left
mov ax, 0x0720
mov cx, 2000
cld 
rep stosw
pop di
pop cx
pop ax
pop es
ret

strlen:
      push bp
      mov bp,sp
      push es
      push cx
      push di
      les di, [bp+4]       ;point es:di to string
      mov cx, 0xffff 
      xor al, al 
      repne scasb 
      mov ax, 0xffff 
      sub ax, cx 
      dec ax      ;lenght in ax
      pop di
      pop cx
      pop es
      pop bp
      ret 4

print_border:   ;this function would just print borders (top/bottom)
push bp
mov bp,sp
push ax
push di
push es
      mov ax,0xb800
      mov es,ax
      mov al,80
      mul byte[bp+6]       ;y_pos* 80
      add ax,[bp+4]         ;+x_pos
      shl ax,1              ;*2
      mov di,ax
      mov al,'='    ;border to print
      mov ah,5   ;attribute
      mov cx,80      ;columns
      l2:
      stosw
      loop l2
pop es
pop di
pop ax
pop bp
ret 4

print_border2:   ;this function would just print borders (right/left)
push bp
mov bp,sp
push ax
push di
push es
      mov ax,0xb800
      mov es,ax
      mov al,80
      mul byte[bp+6]       ;y_pos* 80
      add ax,[bp+4]         ;+x_pos
      shl ax,1              ;*2
      mov di,ax
      mov al,'='    ;border to print
      mov ah,5   ;attribute
      mov cx,22      ;columns
      l3:
      mov word[es:di],ax
      add di,160
      loop l3
pop es
pop di
pop ax
pop bp
ret 4
printstr:
    push bp
    mov bp,sp
    push ax
    push bx
    push dx
    push cx
    push si
      mov cx,[bp+8]     ;lenght
      mov dx, [bp+4] ;location
      mov ax, [bp+6]    ;address of msg
      mov si,ax
      mov ah, 0x13  ;service
      mov al, 1     ;subservice
      mov bh, 0 
      mov bl, [bp+10]   ;attribute
      mov bp,si
      push cs
      pop es
      int 0x10
    pop si
    pop cx
    pop dx
    pop bx
    pop ax
    pop bp
    ret 
print_board:
call clrscr
;print scores string
    ;first player's score
        push ds
        mov ax,score_str    ;string to display
        push ax
        call strlen
        push word 10    ;attribute
        push ax
        mov ax,score_str
        push ax
        push 0x0000     ;row/col
        call printstr
        add sp,8
        call print_score_p1
    ;second player's score
        push ds
        mov ax,score_str    ;string to display
        push ax
        call strlen
        push word 10
        push ax
        mov ax,score_str
        push ax
        push 0x0046     ;row/col
        call printstr
        add sp,8
        call print_score_p2
;printing names on top
    ;first name
        push ds
         mov ax,player1   ;name of player1 (left)
         push ax
         call strlen
         push word 14        ;attribute
         push ax        ;lenght
         mov ax,player1
         push ax
         push 0x0011 ;row/col number
         call printstr
         add sp, 8      ;clean the stack
    ;second name
        push ds
         mov ax,player2   ;name of player1 (right)
         push ax
         call strlen
         push word 14        ;attribute
         push ax        ;lenght
         mov ax,player2
         push ax
         push 0x0036 ;row/col number
         call printstr
         add sp, 8      ;clean the stack

    top_border2:
         mov ax,1    ;row
         push ax
         mov ax,0    ;col
         push ax
         call print_border       ;for top border
    
    bottom_border:
         mov ax,24    ;row
         push ax
         mov ax,0    ;col
         push ax
         call print_border       ;for bottom border
    left_border:
         mov ax,2    ;row
         push ax
         mov ax,0    ;col
         push ax
         call print_border2       ;for right border    
    right_border:
         mov ax,2    ;row
         push ax
         mov ax,79    ;col
         push ax
         call print_border2       ;for left border            
ret
print_menu:
    
    print_msgs:
      ;first one
         push ds
         mov ax,intro   ;intro msg
         push ax
         call strlen
         push word 14        ;attribute
         push ax        ;lenght
         mov ax,intro
         push ax
         push 0x0713 ;row/col number
         call printstr
         add sp, 8      ;clean the stack    
      ;second one
         push ds
         mov ax,devs   ;devs msg
         push ax
         call strlen
         push word 0x07     ;attribute
         push ax        ;lenght
         mov ax,devs
         push ax
         push 0x0819 ;row/col number
         call printstr
         add sp, 8 
      ;third one
         push ds
         mov ax,dev1   ;dev1 msg
         push ax
         call strlen
         push word 0x09    ;attribute
         push ax        ;lenght
         mov ax,dev1
         push ax
         push 0x0921 ;row/col number
         call printstr
         add sp, 8 
       ;fourth one
         push ds
         mov ax,dev2   ;dev2 msg
         push ax
         call strlen
         push word 0x09    ;attribute
         push ax        ;lenght
         mov ax,dev2
         push ax
         push 0x0A21 ;row/col number
         call printstr
         add sp, 8
       ;fifth one
         push ds
         mov ax,cont1   ;continue msg
         push ax
         call strlen
         push word 14    ;attribute
         push ax        ;lenght
         mov ax,cont1
         push ax
         push 0x0B19 ;row/col number
         call printstr
         add sp, 8
       ;sxith one
         push ds
         mov ax,msg1   ;exit msg
         push ax
         call strlen
         push word 4    ;attribute
         push ax        ;lenght
         mov ax,msg1
         push ax
         push 0x0C19 ;row/col number
         call printstr
         add sp, 8
       ;seventh one
         push ds
         mov ax,pause_msg   ;pause msg
         push ax
         call strlen
         push word 0xD    ;attribute
         push ax        ;lenght
         mov ax,pause_msg
         push ax
         push 0x0D19 ;row/col number
         call printstr
         add sp, 8
       ;instructions
        push ds
         mov ax,movement_str   ;inst. str
         push ax
         call strlen
         push word 4    ;attribute
         push ax        ;lenght
         mov ax,movement_str
         push ax
         push 0x0E19 ;row/col number
         call printstr
         add sp, 8

ret

input_name:
push bp
mov bp,sp
    mov di, [bp+4]          ;where the name will be stored
    xor cx, cx           ;to count number of characters

read_loop:
    mov ah, 0x00            
    int 0x16           ;wait for keypress

    cmp al, 0x0D           ;until user presses enter
    je done
    mov [di], al            ;store in di/memmory location of name
    inc di                  ;di to next location
    inc cx            
mov dl, al              ;moving to dl bcz its by default for 0x02
    mov ah, 0x02            ;service to display characters
    int 0x21 
    cmp cx, 10              ;limit upto 10 characters per name
    je done      

    jmp read_loop

done:
    mov byte [di], 0x00
    pop bp
    ret 2

input_caller:
push bp
mov bp,sp
push ax
    push ds    
    mov ax,[bp+4]       ;enter msg
    push ax
    call strlen
    push word 14    ;attribute
    push ax        ;lenght
    mov ax,[bp+4]      ;display enter msg
    push ax
    push 0x0200     ;row/col number
    call printstr
    add sp, 8
    mov ax,[bp+6]       ;palyer name string address
    push ax     ;address to store the name
    call input_name
pop ax
pop bp
ret 4

padel_print_func_p1:
push bp
push ax
push bx
push dx
push cx
push si
    mov cx,[paddle_height]       ;height... means number of rows
    mov dx, [pos_player1]       ;row/col
    ;mov si, paddelll          ;si will hold paddelll address
    mov bl, 0x22      ;attribute

    mov ah, 0x13         
    mov al, 1            ;subservice cursor
    push cs
    pop es               
    mov bp, paddelll
    paddle_loop:
         push cx
         mov cx, 1            ;one character per row
         int 0x10             ;
         pop cx
         add dh, 1            ;move to next row
         loop paddle_loop   ;untill padel height

pop si
pop cx
pop dx
pop bx
pop ax
pop bp
ret 

padel_print_func_p2:
push bp
push ax
push bx
push dx
push cx
push si
    mov cx,[paddle_height]       ;height... means number of rows
    mov dx, [pos_player2]       ;row/col
    ;mov si, paddelll          ;si will hold paddelll address
    mov bl, 0x44      ;attribute

    mov ah, 0x13         
    mov al, 1            ;subservice cursor
    push cs
    pop es               
    mov bp, paddelll
    paddle_loop2:
         push cx
         mov cx, 1            ;one character per row
         int 0x10             ;
         pop cx
         add dh, 1            ;move to next row
         loop paddle_loop2   ;untill padel height

pop si
pop cx
pop dx
pop bx
pop ax
pop bp
ret 

clr_padel:
push cx
push dx
push bx
push bp
push es
push ax
    mov cx,[paddle_height]
    mov dx,ax   ;position
    mov al, 01      ;subservice
    mov ah, 0x13
    mov bl,0        ;attribute (black on black)
    mov bp,erase_char   ;loading space for clearing
    push cs
    pop es
    erase_loop:
        push cx
        mov cx, 1                 ;one space per row
        int 0x10
        pop cx
        add dh, 1                 ;move to next row
        loop erase_loop
pop ax
pop es
pop bp
pop bx
pop dx
pop cx
ret
move_paddle_up:
    cmp ah,2        ;make sure it does not go out bound
    jle no_move_up
    call clr_padel
    sub ah,1    ;one row up
    no_move_up:
    ret    

move_paddle_down:
push dx
    mov dl, ah           ;starting row
    add dl, [paddle_height] ;calculate ending row
    cmp dl, 24           ;making sure it doesn't go beyond row 24
    jge no_move_down 
    call clr_padel
    add ah,1    ;one row down
    no_move_down:
    pop dx
    ret 

move_padel:
push ax

moverr:
    mov ah,0x01
	int 0x16
	jz exit_input
    mov ah, 0x00
    int 0x16
    cmp al, 0x1B ;if ESC then exit
    je exit_input2
    cmp al,'p'
    je pause_game
    cmp al,'W'             ;check for W
    je move_up_p1

    cmp al,'S'             ;check for S
    je move_down_p1

    cmp al,'w'             ;check for w
    je move_up_p1

    cmp al,'s'             ;check for s
    je move_down_p1

    ;as there is no ASCII for arrow keys so we will compare scan code in ah
    cmp ah,0x48        ;as scan code in ah
    je move_up_p2   ;check for upper arrow

    cmp ah,0x50
    je move_down_p2   ;check for lower arrow

exit_input:
    pop ax
    ret
exit_input2:
    pop ax
    pop bp  ;removing the return address from stack
    jmp exit
pause_game:
    mov ah, 0x00
    int 0x16
    cmp al,'p'
    je exit_input
    jmp pause_game   ;to take input again

;creating these labels to pass the correct positon of each player's padel
  move_up_p1:
     
     mov ax,[pos_player1]    ;padel position
     call move_paddle_up    ;for player 1
     mov [pos_player1],ax   ;update padel position
     call padel_print_func_p1

     jmp exit_input     ;to correctly return
  move_down_p1:
     mov ax,[pos_player1]    ;padel position
     call move_paddle_down    ;for player 1
     mov [pos_player1],ax   ;update padel position
     call padel_print_func_p1

     jmp exit_input     ;to correctly return

  move_up_p2:
     mov ax,[pos_player2]    ;padel position
     call move_paddle_up    ;for player 2
     mov [pos_player2],ax   ;update padel position
     call padel_print_func_p2

     jmp exit_input     ;to correctly return

  move_down_p2:
     mov ax,[pos_player2]    ;padel position
     call move_paddle_down    ;for player 2
     mov [pos_player2],ax   ;update padel position
     call padel_print_func_p2

     jmp exit_input     ;to correctly return
score_check:
     push ax
     mov ax,[max_score]
     cmp ax,[p1_score]   ;compares with player1 score
     je win1
     cmp ax,[p2_score]   ;now with second player
     je win2
     pop ax
     ret
win1:
         push ds
         call clrscr
         mov ax,player1   ;name string
         push ax
         call strlen
         push word 2    ;attribute
         push ax        ;lenght
         mov ax,player1
         push ax
         push word 2084 ;screen pos
         call printstr
         add sp, 8
        
         jmp prnt_has_won
win2:
    push ds
         call clrscr
         mov ax,player2   ;name string
         push ax
         call strlen
         push word 2    ;attribute
         push ax        ;lenght
         mov ax,player2
         push ax
         push word 2084 ;screen pos
         call printstr
         add sp, 8
         jmp prnt_has_won
prnt_has_won:
         push word 9    ;attribute
         push word 14        ;lenght
         mov ax,win_msg1    ;winner string
         push ax
         push word 2158 ;screen pos
         call printstr
         add sp, 8
         pop ax ;remove address
         jmp exit
start:
     call clrscr

     name_input:  ;taking the name as input from 
        mov ax,player1  ;where the name will be stored
        push ax
        mov ax,pp1
        push ax
        call input_caller

        call clrscr

        mov ax,player2  ;where the name will be stored
        push ax
        mov ax,pp2
        push ax
        call input_caller

        
    
    call print_menu
     mov ah, 0       ;wait for key
     int 0x16
     cmp al, 0x1B ;if ESC then exit
     je exit
     call print_board
    
    ;padel for plpayer 1
       call padel_print_func_p1    

    ;padel for plpayer 2       
       call padel_print_func_p2   
    mov si,1
    call print_ball
    game_loop:
       call score_check
       call print_score_p1
       call print_score_p2    
       call move_padel
	   		call delay
            call delay
       call move_ball
       cmp si,0
       jne game_loop

mov ah,0x1
int 0x21

exit:
mov ax,0x4c00
int 0x21