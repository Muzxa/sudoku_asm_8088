[org 0x0100]

jmp start

off_screen_buffer: times 2000 dw 0

sudoku:         db 'SUDOKU', 0
play_game:      db 'PLAY  GAME', 0
exit:           db 'EXIT', 0
level_select:   db 'LEVEL SELECT', 0
easy:           db 'EASY', 0
medium:         db 'MEDIUM', 0
hard:           db 'HARD', 0
difficulty_s:   db 'DIFFICULTY: ', 0
score_s:        db 'SCORE: ', 0
mistakes_s:     db 'MISTAKES: ', 0
timer_s:        db 'TIMER: ', 0
notes_s:        db 'NOTES: ', 0
off_s           db 'OFF', 0
on_s            db 'ON', 0
game_over_s:    db 'GAME OVER', 0
time_elapsed_s: db 'TIME ELAPSED:  0:00', 0

difficulty:   dw 0
score:        dw 0
mistakes:     dw 0
max_mistakes: db '0'
timer:        db '0:00', 0
mode_text:    dw 0
mode_number   dw 1

gen_flag: dw 0
scroll_flag: dw 1

draw_line:

  push bp
  mov  bp, sp
  push ax
  push cx
  push es
  push di

  mov  ax, ds
  mov  es, ax

  mov  ax, 80
  mul  byte [bp + 4] ;MULTIPLY BY VERTICAL COORDINATES
  shl ax, 1

  mov  cx, [bp + 6]  ;ADD HORIZONTAL COORDINATES, TWICE
  shl cx, 1
  add ax, cx

  mov  di, ax        ;MOVE POINTER LOCATION TO di
  add di, off_screen_buffer
  mov  cx, [bp + 10] ;MOVE LENGTH INTO si

  mov  ax, [bp + 8]  ;MOVE THE FORMAT INTO ax

  l_print_loop:
    mov [es:di], ax
    add di, [bp + 12]
  loop l_print_loop

  pop di
  pop es
  pop cx
  pop ax
  pop bp

ret 10

print_text:
  ;FUNCTION NAME: PRINT_TEXT

  ;PASSED PARAMETERS
  ;[bp + 12][1ST PARAM] - STRING MODE (0) OR NUMBER MODE (1)
  ;[bp + 10][2ND PARAM] - TEXT TO PRINT
  ;[bp + 8] [3RD PARAM] - TEXT FORMAT
  ;[bp + 6] [4TH PARAM] - X-COORDINATE OF STARTING POINT
  ;[bp + 4] [5TH PARAM] - Y-COORDINATE OF STARTING POINT

  ;LOCAL VARIABLES
  ;[bp - 2] - DIVIDEND

  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push bp
  mov  bp, sp

  sub sp, 2
  mov word [bp - 2], 10
  
  push ax
  push cx
  push dx
  push es
  push di
  push si

  ;FUNCTION START
  cmp word [bp + 12], 1
  je number_mode

  text_mode:
    mov  ax, ds
    mov  es, ax

    ;SCREEN LOCATION CALCULATION
    mov  ax, 80
    shl  ax, 1
    mul  byte [bp + 4] ;MULTIPLY BY VERTICAL COORDINATES
    mov di, [bp + 6]   ;ADD HORIZONTAL COORDINATES, TWICE
    shl di, 1
    add ax, di
    mov  di, ax        ;MOVE POINTER LOCATION TO di
    add  di, off_screen_buffer

    mov  si, [bp + 10] ;MOVE THE START OF WORD INTO si
    mov  ax, [bp + 8]  ;MOVE THE FORMAT INTO ax
    
    text_print_loop:
      mov al, [si]
      mov [es:di], ax
      add di, 2
      inc si
      cmp byte [si], 0
  jne text_print_loop
  jmp print_text_end

  number_mode:

    mov si, [bp + 10]
    mov ax, [si]
    xor cx, cx

    stack_push_loop:

      div byte [bp - 2]
      mov dl, ah
      xor dh, dh
      add dx, '0'
      push dx
      xor ah, ah
      inc cx
      cmp ax, 0
    jne stack_push_loop

    mov ax, sp
    mov dx, cx

    number_print_loop:
      
      push word [mode_text]
      push ax
      push word [bp + 8]
      push word [bp + 6]
      inc word [bp + 6]
      push word [bp + 4]
      call print_text
      add ax, 2
    loop number_print_loop

    sub word [bp + 6], dx
    shl dx, 1
    add sp, dx

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  print_text_end:

  pop si
  pop di
  pop es
  pop dx
  pop cx
  pop ax
  add sp, 2
  pop bp

ret 10

move_buffer_to_screen:

  push ds
  push si
  push es
  push di
  push ax
  push cx

  mov si, off_screen_buffer
  mov ax, 0xb800
  mov es, ax
  mov di, 0
  mov cx, 2000

  cld
  rep movsw

  pop cx
  pop ax
  pop di
  pop es
  pop si
  pop ds
ret

sleep: 

  push bp
  mov  bp, sp

  push   cx
  mov    cx, [bp + 4]
  delay: loop delay
  pop    cx
  pop    bp

ret 2

draw_bg_to_buffer:   
  push bp
  mov  bp, sp

  push es
  push ax
  push di
  push si
  push cx

  mov ax, ds
  mov es, ax
  mov di, off_screen_buffer

  mov ax, [bp + 6]
  mov cx, 2000

  cld 
  rep stosw

  pop cx
  pop si
  pop di
  pop ax
  pop es
  pop bp
ret 4

draw_bg:   
  push bp
  mov  bp, sp

  push es
  push ax
  push di
  push si
  push cx

  mov ax, 0xb800
  mov es, ax
  mov di, 0

  mov ax, [bp + 6]
  mov cx, 2000

  cld 
  rep stosw

  pop cx
  pop si
  pop di
  pop ax
  pop es
  pop bp

ret 4

draw_mm:

  push bp
  mov  bp, sp

  push ax
  push es

  mov ax, 0xb800
  mov es, ax

  mov ah, 01110000b

  push word [mode_text]
  push sudoku
  push word 0111000000000000b
  push word 37
  push word 11
  call print_text

  cmp word [gen_flag], 0
  jne skip_pgf

  mov ah, 11111100b
  
  skip_pgf:
  
  push word [mode_text]
  push play_game
  push ax
  push word 35
  push word 13
  call print_text

  mov ah, 01110000b

  cmp word [gen_flag], 1
  jne skip_ef

  mov ah, 11111100b
  
  skip_ef:

  push word [mode_text]
  push exit
  push ax
  push word 38
  push word 14
  call print_text

  call move_buffer_to_screen

  pop es
  pop ax
  pop bp

  ret

draw_ls:

  push bp
  mov  bp, sp

  push ax
  push es

  mov ax, 0xb800
  mov es, ax

  mov ah, 01110000b

  push word [mode_text]
  push level_select
  push word 0111000000000000b
  push word 34
  push word 10
  call print_text

  cmp word [gen_flag], 0
  jne skip_lsf

  mov ah, 11111100b
  mov word [difficulty], hard
  
  skip_lsf:
  push word [mode_text]
  push easy
  push ax
  push word 38
  push word 12
  call print_text

  mov ah, 01110000b

  cmp word [gen_flag], 1
  jne skip_esf

  mov ah, 11111100b
  mov word [difficulty], easy
  
  skip_esf:
  push word [mode_text]
  push medium
  push ax
  push word 37
  push word 13
  call print_text

  mov ah, 01110000b

  cmp word [gen_flag], 2
  jne skip_hf

  mov ah, 11111100b
  mov word [difficulty], medium
  
  skip_hf:
  push word [mode_text]
  push hard
  push ax
  push word 38
  push word 14
  call print_text

  call move_buffer_to_screen

  pop es
  pop ax

  pop bp

ret

draw_es:
  ;NAME: draw_es (DRAW END SCREEN)
  
  ;PASSED PARAMETERS
  ;N/A
  
  ;LOCAL VARIABLES
  ;[bp - 2] - X-COORDINATE OF STARTING POINT
  ;[bp - 4] - Y-COORDINATE OF STARTING POINT
  ;[bp - 6] - REGULAR TEXT FORMAT
  ;[bp - 8] - CURRENT SELECTION TEXT FORMAT
  
  ;PARAMETERS FOR PRINT_TEXT FUNCTION
  ;[bp + 10][1ST PARAM] - TEXT TO PRINT
  ;[bp + 8] [2ND PARAM] - TEXT FORMAT
  ;[bp + 6] [3RD PARAM] - X-COORDINATE OF STARTING POINT
  ;[bp + 4] [4TH PARAM] - Y-COORDINATE OF STARTING POINT
  
  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push bp
  mov  bp, sp
  sub sp, 8
  push ax
  push es
  
  mov word [bp - 2], 37
  mov word [bp - 4], 11
  mov word [bp - 6], 0111000000000000b
  mov word [bp - 8], 1111110000000000b
  
  ;FUNCTION START
  mov ax, 0xb800
  mov es, ax
  mov ah, 01110000b
  
  push word [mode_text]
  push word game_over_s
  push word [bp - 6]
  push word [bp - 2]
  push word [bp - 4]
  call print_text

  add word [bp - 4], 2
  sub word [bp - 2], 5

  push word [mode_text]
  push word time_elapsed_s
  push word [bp - 6]
  push word [bp - 2]
  push word [bp - 4]
  call print_text

  call move_buffer_to_screen
  
  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop es
  pop ax
  add sp, 8
  pop bp

ret

draw_gs_te:
  ;NAME: DRAW_GS_TE (DRAW GAMESPACE TEXT ELEMENTS)

  ;PASSED PARAMETERS
  ;[bp + 8] - FORMAT 
  ;[bp + 6] - X-COORDINATE OF VERTICAL DIVIDER LINE 
  ;[bp + 4] - Y-COORDINATE OF HORIZONTAL DIVIDER LINE

  ;LOCAL VARIABLES
  ;[bp - 2] - ROW INDEX OF NEXT AVAILABLE LINE
  ;[bp - 4] - OFFSET FROM STARTING POINT

  ;PARAMETERS FOR PRINT_TEXT FUNCTION
  ;[bp + 10][1ST PARAM] - TEXT TO PRINT
  ;[bp + 8] [2ND PARAM] - TEXT FORMAT
  ;[bp + 6] [3RD PARAM] - X-COORDINATE OF STARTING POINT
  ;[bp + 4] [4TH PARAM] - Y-COORDINATE OF STARTING POINT
   
  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push bp
  mov bp, sp
  sub sp, 4
  push ax
  push bx
  push cx
  push dx

  mov word [bp - 2], 1
  mov word [bp - 4], 5
  
  ;FUNCTION START
  push word [mode_text]
  push difficulty_s ;PRINTING DIFFICULTY STAT
  push word [bp + 8]
  mov dx, [bp + 6]
  add dx, [bp - 4]
  push dx
  push word [bp - 2]
  call print_text

  add word [bp - 2], 1

  push word [mode_text]
  push word [difficulty]
  push word [bp + 8]
  mov dx, [bp + 6]
  add dx, [bp - 4]
  push dx
  push word [bp - 2]
  call print_text

  add word [bp - 2], 2

  push word [mode_text]
  push timer_s       ;PRINTING TIMER STAT
  push word [bp + 8]
  mov dx, [bp + 6]
  add dx, [bp - 4]
  push dx
  push word [bp - 2]
  call print_text

  add word [bp - 2], 1

  push word [mode_text]
  push timer
  push word [bp + 8]
  mov dx, [bp + 6]
  add dx, [bp - 4]
  push dx
  push word [bp - 2]
  call print_text

  add word [bp - 2], 2

  push word [mode_text]
  push score_s       ;PRINTING SCORE STAT
  push word [bp + 8]
  mov dx, [bp + 6]
  add dx, [bp - 4]
  push dx
  push word [bp - 2]
  call print_text

  add word [bp - 2], 1

  push word [mode_number]
  push score
  push word [bp + 8]
  mov dx, [bp + 6]
  add dx, [bp - 4]
  push dx
  push word [bp - 2]
  call print_text

  add word [bp - 2], 2

  push word [mode_text]
  push mistakes_s ;PRINTING MISTAKES STAT
  push word [bp + 8]
  mov dx, [bp + 6]
  add dx, [bp - 4]
  push dx
  push word [bp - 2]
  call print_text

  add word [bp - 2], 1

  push word [mode_number]
  push mistakes
  push word [bp + 8]
  mov dx, [bp + 6]
  add dx, [bp - 4]
  push dx
  push word [bp - 2]
  call print_text

  add word [bp - 2], 2

  push word [mode_text]
  push notes_s       ;PRINTING NOTES STAT
  push word [bp + 8]
  mov dx, [bp + 6]
  add dx, [bp - 4]
  push dx
  push word [bp - 2]
  call print_text

  add word [bp - 2], 1

  push word [mode_text]
  push off_s
  push word [bp + 8]
  mov dx, [bp + 6]
  add dx, [bp - 4]
  push dx
  push word [bp - 2]
  call print_text

  call move_buffer_to_screen
  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop dx
  pop cx
  pop bx
  pop ax
  add sp, 4
  pop bp

  ret 6

draw_gs:
  ;NAME : DRAW_GS (DRAW GAMESPACE)

  ;PASSED PARAMETERS
  ;[bp + 4] - NUMBER OF ROWS TO PRINT

  ;LOCAL VARIABLES
  ;[bp - 2]  - X-COORDINATE OF STARTING POINT
  ;[bp - 4]  - Y-COORDINATE OF STARTING POINT
  ;[bp - 6]  - X-COORDINATE OF VERTICAL DIVIDER LINE
  ;[bp - 8]  - Y-COORDINATE OF HORIZONTAL DIVIDER LINE
  ;[bp - 10] - MAIN LINE FORMAT
  ;[bp - 12] - SUB-LINE FORMAT
  ;[bp - 14] - VERTICAL MAIN LINE LENGTH
  ;[bp - 16] - VERTICAL SUB-LINE LENGTH
  ;[bp - 18] - HORIZONTAL LINE LENGTH

  ;PARAMETERS FOR DRAW_LINE FUNCTION
  ;[bp + 12] [1ST PARAM] - VERTICAL MODE (160) OR HORIZONTAL MODE (2)
  ;[bp + 10] [2ND PARAM] - LENGTH OF THE LINE
  ;[bp + 8]  [3RD PARAM] - FORMAT OF THE LINE
  ;[bp + 6]  [4TH PARAM] - X-COORDINATE OF THE LINE
  ;[bp + 4]  [5TH PARAM] - Y-COORDINATE OF THE LINE

  ;PARAMETERS FOR DRAW_GS_TE (DRAW GAMESPACE TEXT ELEMENTS) FUNCTION
  ;[bp + 8] [1ST PARAM] - FORMAT OF THE LINE
  ;[bp + 6] [2ND PARAM] - X-COORDINATE OF VERTICAL DIVIDER LINE
  ;[bp + 4] [3RD PARAM] - Y-COORDINATE OF HORIZONTAL DIVIDER LINE
  
  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push bp
  mov  bp, sp

  sub sp, 18

  push ax
  push cx
  push dx

  mov word [bp - 2],  0
  mov word [bp - 4],  0
  mov word [bp - 6],  61
  mov word [bp - 8],  25
  mov word [bp - 10], 0000000000000000b
  mov word [bp - 12], 0111000000000000b
  mov word [bp - 18], 59

  mov al, 12
  mul byte [bp + 4]
  mov word [bp - 14], ax ;CALCULATING VERTICAL MAIN LINE LENGTH AND STORING IT

  sub ax, 1
  mov word [bp - 16], ax ;CALCULATING VERTICAL SUB LINE LENGTH AND STORING IT

  ;FUNCTION START
  mov cx, [bp + 4]
  inc cx
  mov ax, [bp - 4]

  draw_main_horizontal_lines: ;LOOP TO DRAW MAIN AND SUB HORIZONTAL LINES

    push word 2
    push word [bp - 18]
    push word [bp - 10]
    push word [bp - 2]
    push ax
    call draw_line
    cmp  cx, 1
    jbe  skip_drawing_subline

    push ax    ;SAVING AX AND CX SINCE I NEED TO USE THEM IN THE INNER LOOP
    push cx
    mov  cx, 2 ;BETWEEN TWO MAIN LINES, THERE ARE TWO SUB-LINES
    
    draw_sub_horizontal_lines:

      add  ax, 4
      push word 2
      push word [bp - 18]
      mov  dx, [bp - 12];CHANGE HERE
      mov  dl, 00101101b
      push dx
      push word [bp - 2]
      push word ax
      call draw_line

    loop draw_sub_horizontal_lines ;INNER LOOP

    pop cx
    pop ax
  
   skip_drawing_subline:
     add ax, 12
  loop draw_main_horizontal_lines ;OUTER LOOP

  mov cx, 4
  mov ax, [bp - 2]

  draw_main_vertical_lines: ;LOOP TO DRAW MAIN AND SUB VERTICAL LINES
    push word 160
    push word [bp - 14]
    push word [bp - 10]
    push word ax
    push word [bp - 4]
    call draw_line

    add ax, 1

    push word 160
    push word [bp - 14]
    push word [bp - 10]
    push word ax
    push word [bp - 4]
    call draw_line

    cmp cx, 1
    jbe skip_drawing_vertical_subline ;IF CX IS 1, THEN NO NEED TO DRAW SUB-LINES SINCE THE LAST MAIN LINE IS THE BORDER

    push ax ;SAVING AX AND CX SINCE I NEED TO USE THEM IN THE INNER LOOP
    push cx

    mov cx, 2 ;BETWEEN TWO MAIN LINES, THERE ARE TWO SUB-LINES
    
    draw_vertical_sub_lines: 

      add ax, 6

      push word 160
      push word [bp - 16]
      mov  dx, [bp - 12]
      mov  dl, 01111100b
      push dx
      push ax

      mov dx, [bp - 4]
      inc dx

      push dx
      call draw_line

    loop draw_vertical_sub_lines ;INNER LOOP

    pop cx
    pop ax
  
   skip_drawing_vertical_subline: 
     add ax, 18
  loop draw_main_vertical_lines ;OUTER LOOP


  push word 2         ;DRAWING A LINE FOR COSMETIC PURPOSES
  push word [bp - 18]
  push word [bp - 10]
  push word [bp - 2]
  mov  dx, [bp - 4]
  add  dx, 12
  push dx
  call draw_line

  push word 160         ;DRAWING THE VERTICAL DIVIDER
  push word [bp - 8]
  push word [bp - 10]
  push word [bp - 6]
  push word 0
  call draw_line

  mov dx, [bp - 6]
  inc dx

  push word 160        
  push word [bp - 8]
  push word [bp - 10]
  push dx
  push word 0
  call draw_line
  
  push word [bp - 12]
  push word [bp - 6] 
  push word [bp - 8] 
  call draw_gs_te

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop dx
  pop cx
  pop ax
  add sp, 18
  pop bp

  ret 2

  clear_screen:

    push bp
    mov bp, sp

    push 0000111100000000b
    push 01h
    call draw_bg

    pop bp
  ret

  refresh_screen:

    push bp
    mov bp, sp

    push 0111111100000000b
    push 01h
    call draw_bg
    
    pop bp
  ret

  refresh_buffer:

    push bp
    mov bp, sp

    push 0111111100000000b
    push 01h
    call draw_bg_to_buffer
    
    pop bp
  ret

  clear_buffer:

    push bp
    mov bp, sp

    push 0000111100000000b
    push 01h
    call draw_bg_to_buffer

    pop bp
  ret

start:

  mov ah, 00h
  mov al, 03h
  int 10h

  main_menu_ol:
  mov word [gen_flag], 0
  call refresh_buffer
  call draw_mm

  main_menu_il:
  mov ah, 0x00
  int 16h

  cmp ah, 0x50
  je  flip_gen_flag

  cmp ah, 0x48
  jne end_mm_il

  flip_gen_flag:
    xor  word [gen_flag], 0x0001
    call draw_mm

  end_mm_il:
    cmp ah, 0x1c
    jne main_menu_il

  cmp word [gen_flag], 0x0001
  je  game_end

  call refresh_buffer
  call draw_ls
  mov word [gen_flag], 0
  
  ls_il:
    mov ah, 0x00
    int 16h

    cmp ah, 0x50
    je  inc_gen_flag

    cmp ah, 0x48
    je  dec_gen_flag

    inc_gen_flag:
      inc word [gen_flag]
      cmp word [gen_flag], 2
      jna skip_ls_il
      mov word [gen_flag], 0
      jmp skip_ls_il

    dec_gen_flag:
      dec word [gen_flag]
      cmp word [gen_flag], 0
      jnl  skip_ls_il
      mov word [gen_flag], 2

    skip_ls_il:
      call draw_ls
      cmp ah, 0x01
      je  main_menu_ol
      cmp ah, 0x1c
      jne ls_il

  call refresh_buffer
  push word 2
  call draw_gs

  game_loop:
    mov ah, 0x00
    int 16h                    
                               
    cmp ah, 0x01               
    je  main_menu_ol    
    
    cmp ah, 0x48
    je flip_scroll_flag

    cmp ah, 0x50
    jne enter_key_check

    flip_scroll_flag:
     xor word [scroll_flag], 1
    
    draw_gs_game_loop:
     call refresh_buffer
     inc word [scroll_flag]
     push word [scroll_flag]
     dec word [scroll_flag]
     call draw_gs

    enter_key_check:
     cmp ah, 0x1c
  jne game_loop 
     
  call refresh_buffer
  call draw_es

  es_il:
    xor ah, ah
    int 16h

    cmp ah, 0x1c
  jne es_il
  jmp main_menu_ol

game_end:
  call clear_screen

  mov ax, 0x4c00
  int 0x21

;COMMON SCAN CODES
; ENTER - 0x1C
; BACKSPACE - 0x0E
; SPACE - 0x39
; UP ARROW - 0x48
; DOWN ARROW - 0x50