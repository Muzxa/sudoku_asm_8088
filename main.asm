[org 0x0100]

jmp start

off_screen_buffer:        times 2000 dw 0
overflow_buffer:          times 20 dw 0

sudoku:                   db 'SUDOKU', 0
play_game:                db 'PLAY', 0
exit_s:                   db 'EXIT', 0
level_select:             db 'LEVEL SELECT', 0
easy:                     db 'EASY', 0
medium:                   db 'MEDIUM', 0
hard:                     db 'HARD', 0
difficulty_s:             db 'DIFFICULTY: ', 0
score_s:                  db 'SCORE: ', 0
mistakes_s:               db 'MISTAKES: ', 0
timer_s:                  db 'TIME: ', 0
notes_s:                  db 'NOTES: ', 0
notes_s_theme:            dw main_theme
off_s                     db 'OFF', 0
on_s                      db 'ON', 0
game_over_s:              db 'GAME OVER', 0
time_elapsed_s:           db 'TIME REMAINING: ', 0
number_cards_s:           db 'NUMBER CARDS', 0
colon:                    db ':', 0
pipe:                     db '|', 0

difficulty:               dw 0
score:                    dw 0
score_multiplier:         dw 1
mistakes_left:            dw 0
current_notes_setting:    dw 0

number_cards:             dw 0, 0, 0, 0, 0, 0, 0, 0, 0
curr_index:               dw 0

;FLAGS
gen_flag:                 dw 0
scroll_flag:              dw 1
ncs_flag:                 dw 0
terminate_flag:           dw 0
notes_flag:               dw 0

main_theme:               equ 0000111100000000b
main_notes_theme:         equ 0000010000000000b
notes_background:         equ 0100111100000000b
current_selection_theme:  equ 1000010000000000b
highlight_theme:          equ 0000010000000000b
clear_screen_theme:       equ 0000011100000000b
horizontal_subline_char:  equ '*'
vertical_subline_char:    equ '*'
cursor_sel_attr_byte:     equ 11001111b
cursor_icon:              equ '_'
line_theme:               equ 0111000000000000b

tickcount:                dw 0
seconds:                  dw 59
minutes:                  dw 9
zero:                     dw 0
temp_value:               dw 0

oldisr:                   dd 0
oldkbisr:                 dd 0

cursor_row:               dw 0
cursor_col:               dw 0

right:                    equ 1
left:                     equ 2
up:                       equ 3
down:                     equ 4
mode_text:                equ 0
mode_number:              equ 1

current_grid_ptr:         dw 0
solution_grid_ptr:        dw 0

;GRIDS
easy_grid:                db 0,3,2,0,0,0,0,5,7
                          db 7,4,5,0,1,8,0,0,6
                          db 1,0,0,7,0,3,2,8,0
                          db 3,0,8,0,0,0,4,0,5
                          db 4,0,9,1,0,0,0,0,0
                          db 0,6,1,0,0,4,9,0,0
                          db 2,5,7,6,0,0,0,0,1
                          db 9,8,0,0,3,0,6,0,0
                          db 0,1,0,8,0,0,0,0,9

medium_grid:              db 4,0,0,0,6,0,9,0,0
                          db 0,0,0,0,4,3,6,8,0
                          db 0,8,0,7,0,1,2,0,4
                          db 0,0,0,0,8,0,4,0,0
                          db 0,9,0,0,0,7,1,6,0
                          db 7,6,0,0,1,0,0,3,0
                          db 0,0,1,0,0,9,0,0,0
                          db 3,0,9,0,0,0,0,0,0
                          db 0,5,6,0,0,0,8,0,1

hard_grid:                db 0,5,1,0,9,0,0,2,6
                          db 9,0,0,8,1,7,0,0,0
                          db 4,0,0,0,0,0,0,0,1
                          db 0,6,0,4,0,2,0,1,0
                          db 3,0,0,0,0,1,0,0,0
                          db 0,0,7,0,3,8,2,0,4
                          db 0,0,0,0,0,0,4,7,0
                          db 0,3,0,5,0,0,1,0,0
                          db 0,0,0,7,0,0,5,3,0

curr_grid:                times 81 db 0

easy_grid_sol:            db 8,3,2,4,9,6,1,5,7
                          db 7,4,5,2,1,8,3,9,6
                          db 1,9,6,7,5,3,2,8,4
                          db 3,7,8,9,6,2,4,1,5
                          db 4,2,9,1,8,5,7,6,3
                          db 5,6,1,3,7,4,9,2,8
                          db 2,5,7,6,4,9,8,3,1
                          db 9,8,4,5,3,1,6,7,2
                          db 6,1,3,8,2,7,5,4,9

medium_grid_sol:          db 4,2,7,8,6,5,9,1,3
                          db 9,1,5,2,4,3,6,8,7
                          db 6,8,3,7,9,1,2,5,4
                          db 1,3,2,5,8,6,4,7,9
                          db 5,9,8,4,3,7,1,6,2
                          db 7,6,4,9,1,2,5,3,8
                          db 8,7,1,6,2,9,3,4,5
                          db 3,4,9,1,5,8,7,2,6
                          db 2,5,6,3,7,4,8,9,1

hard_grid_sol:            db 8,5,1,3,9,4,7,2,6
                          db 9,2,6,8,1,7,3,4,5
                          db 4,7,3,2,6,5,8,9,1
                          db 5,6,8,4,7,2,9,1,3
                          db 3,4,2,9,5,1,6,8,7
                          db 1,9,7,6,3,8,2,5,4
                          db 6,8,5,1,2,3,4,7,9
                          db 7,3,4,5,8,9,1,6,2
                          db 2,1,9,7,4,6,5,3,8

;UNDO
undo_stack:               times 81*4 db 0
undo_stack_ptr:           dw 242

;NOTES
notes_array:              times 81*9 db 0

;MUSIC
track:                    dw 6000, 4000, 5000, 4000, 6000, 4000, 5000, 6000 ; THIS IS THE TRACK FREQUENCY ARRAY
track_d:                  dw 5, 6, 7, 6, 5, 6, 7, 6                         ; THIS IS THE TRACK DELAY ARRAY
track_s:                  dw 8                                              ; THIS IS THE TRACK SIZE

correct_input_track:      dw 3500, 3000
correct_input_track_d:    dw 4, 4
correct_input_track_s:    dw 2

incorrect_input_track:    dw 6500, 6000
incorrect_input_track_d:  dw 4, 4
incorrect_input_track_s:  dw 2

row_col_track:            dw 3000, 4000, 5500    
row_col_track_d:          dw 4, 4, 7                  
row_col_track_s:          dw 3

DELAY:  ;delay of 1/18.2 seconds
        push bp
        mov bp, sp
        push cx
        push ax

        DF_l1:                             
            cmp word[bp+4], 0
            je DF_end
            mov ax, 0x0001
            DF_l2:
            mov cx, 0xFFFF
                DS_l3: loop DS_l3     
            dec ax
            jnz DF_l2
            dec word[bp+4]
            jmp DF_l1
        DF_end:
        pop ax
        pop cx
        pop bp
        ret 2
sound:
        ;load the counter 2 value for d3
        ;push bx

        PUSH BP
        MOV BP, SP
        PUSHA

        mov ax, [bp + 6]
        out 42h, al
        mov al, ah
        out 42h, al
        
        ;turn the speaker on
        in al, 61h
        mov ah ,al
        or al, 3h
        out 61h, al
        mov bx, 5

        mov bx,[bp+4]
        push bx
        call DELAY
        
        mov al, ah
        out 61h, al

        POPA
        POP BP
ret 4

play_track:
  ;FUNCTION NAME: PLAY TRACK

  ;PASSED PARAMETERS
  ;[bp + 8] [1SR PARAM] - POINTER TO TRACK FREQUENCY ARRAY
  ;[bp + 6] [2ND PARAM] - POINTER TO TRACK DELAY ARRAY
  ;[bp + 4] [3RD PARAM] - TRACK ARRAY SIZE

  ;LOCAL VARIABLES: N/A

  push bp
  mov bp, sp
  push ax
  push cx
  push si
  push di

  ;FUNCTION START
  mov word cx, [bp + 4]
  mov si, [bp + 6]
  mov di, [bp + 8]

  track_loop:
    push word [di]
    push word [si]
    call sound
    add si, 2
    add di, 2
  loop track_loop

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop di
  pop si
  pop cx
  pop ax
  pop bp
ret 6

toggle_note:
  ;FUNCTION NAME: TOGGLE NOTE

  ;PASSED PARAMETERS
  ;[bp + 8]  [2ND PARAM] - NOTE VALUE (1 - 9)
  ;[bp + 6]  [3RD PARAM] - CURSOR ROW
  ;[bp + 4]  [4TH PARAM] - CURSOR COLUMN

  ;LOCAL VARIABLES: N/A
  push bp
  mov bp, sp
  pusha

  mov ax, 81
  mul byte [bp + 6]
  mov bx, ax
  mov ax, 9
  mul byte [bp + 4]
  add bx, ax
  add bx, [bp + 8]
  dec bx
  add bx, notes_array

  xor cx, cx
  mov cx, [bp + 8]
  xor byte [bx], cl

popa
pop bp
ret 6

return_cursor_location_on_screen:
  ;FUNCTION NAME: RETURN CURSOR LOCATION ON SCREEN

  ;PASSED PARAMETERS
  ;[bp + 8] [1ST PARAM] - RETURN VALUE
  ;[bp + 6] [2ND PARAM] - CURSOR ROW
  ;[bp + 4] [3RD PARAM] - CURSOR COLUMN

  ;LOCAL VARIABLES
  ;[bp - 2] - TEMP CURSOR ROW
  ;[bp - 4] - TEMP CURSOR COLUMN

  push bp
  mov bp, sp
  sub sp, 4
  push ax
  push bx
  push cx
  push dx

  ;FUNCTION START
  mov ax, [bp + 6]
  mov [bp - 2], ax

  cmp word [bp + 6], 6
  jb else_rclos_3
  sub word [bp - 2], 6

  else_rclos_3:
  mov ax, [bp + 4]
  mov [bp - 4], ax

  mov ax, 4
  mul byte [bp - 2]
  mov cx, ax

  mov ax, 6
  mul byte [bp - 4]
  mov dx, ax

  cmp word [bp - 4], 3
  jl else_rclos_1
  inc dx

  else_rclos_1:
  cmp word [bp - 4], 6
  jl else_rclos_2
  inc dx

  else_rclos_2:
  mov ax, 80
  mov bl, 2
  add bl, cl
  mul byte bl
  add ax, 4
  add ax, dx
  shl ax, 1
  mov [bp + 8], ax

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop dx
  pop cx
  pop bx
  pop ax
  add sp, 4
  pop bp
ret 4

check_if_grid_complete:

  ;FUNCTION NAME: CHECK IF GRID COMPLETE

  ;[bp + 6] [1ST PARAM] - RETURN VALUE (0 - FALSE, 1 - TRUE)
  ;[bp + 4] [2ND PARAM] - POINTER TO GRID

  ;LOCAL VARIABLES: N/A

  push bp
  mov bp, sp
  push ax
  push cx
  push es
  push di

  ;FUNCTION START
  mov ax, ds
  mov es, ax

  mov di, number_cards
  xor ax, ax
  mov cx, 10

  cld
  repe scasw
  
  cmp cx, 0
  jg return_0_cigc
  mov word [bp + 6], 1
  jmp end_cigc

  return_0_cigc:
  mov word [bp + 6], 0

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  end_cigc:
  pop di
  pop es
  pop cx
  pop ax
  pop bp
ret 2

print_notes_grid:
  ;FUNCTION NAME: PRINT NOTES GRID

  ;[bp + 8] [1ST PARAM] - BACKGROUND COLOR
  ;[bp + 6] [2ND PARAM] - CURSOR ROW
  ;[bp + 4] [3RD PARAM] - CURSOR COLUMN

  ;LOCAL VARIABLES: N/A

  push bp
  mov bp, sp
  pusha

  ;FUNCTION START
  ;-- SCREEN LOCATION CALCULATION FOR [DS:SI] -- 
  mov ax, 81
  mul byte [bp + 6]
  mov si, ax
  mov ax, 9
  mul byte [bp + 4]
  add si, ax
  add si, notes_array

;-- SCREEN LOCATION CALCULATION FOR [ES:DI] -- 
  push ds
  pop es

  mov ax, [bp + 6]
  cmp ax, 6
  jb else_png_1
  sub ax, 6
  
  else_png_1:
  push 0
  push word ax
  push word [bp + 4]
  call return_cursor_location_on_screen
  pop di
  add di, off_screen_buffer
  sub di, 162

  mov cx, 9
  xor dx, dx

  note_print_loop:
    mov ax, [bp + 8]
    mov al, [ds:si]
    cmp al, 0
    je skip_printing_npl
    add al, '0'
    mov [es:di], ax
    skip_printing_npl:
    inc si
    add di, 2
    inc dx
    cmp dx, 3
    jb stay_on_row
    add di, 154
    xor dx, dx
    stay_on_row:
  loop note_print_loop

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  popa
  pop bp
ret 6

clear_notes:
  ;FUNCTION NAME: CLEAR NOTES
  ;PASSED PARAMETERS: N/A
  ;LOCAL VARIABLES: N/A

  push bp
  mov bp, sp
  push es
  push di
  push ax
  push cx

  ;FUNCTION START
  push ds
  pop es
  xor di, di
  add di, notes_array
  xor ax, ax
  mov cx, 729
  rep stosb

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop cx
  pop ax
  pop di
  pop es
  pop bp
ret

change_notes_grid_bg:
  ;FUNCTION NAME: CHANGE NOTES GRID BACKGROUND

  ;PASSED PARAMETERS
  ;[bp + 6] [1ST PARAM] - BACKGROUND COLOR
  ;[bp + 6] [2ND PARAM] - CURSOR ROW
  ;[bp + 4] [3RD PARAM] - CURSOR COLUMN

  ;LOCAL VARIABLES: N/A

  push bp
  mov bp, sp
  pusha

  ;FUNCTION START
  ;SETTING ES TO VIDEO MEMORY
  push ds
  pop es
  mov ax, [bp + 8]
  mov al, 0

  ;GETTING CURSOR LOCATION AND POPPING IT TO DI
  push 0
  push word [bp + 6]
  push word [bp + 4]
  call return_cursor_location_on_screen
  pop di
  add di, off_screen_buffer

  sub di, 161
  mov [es:di], ah
  add di, 2
  mov [es:di], ah
  add di, 2
  mov [es:di], ah
  add di, 156
  mov [es:di], ah
  add di, 2
  mov [es:di], ah
  add di, 2
  mov [es:di], ah
  add di, 156
  mov [es:di], ah
  add di, 2
  mov [es:di], ah
  add di, 2
  mov [es:di], ah

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  popa
  pop bp
ret 6

grow_undo_stack:
  ;FUNCTION NAME: GROW_UNDO_STACK

  ;PASSED PARAMETERS
  ;[bp + 10][1ST PARAM] - SCORE
  ;[bp + 8] [2ND PARAM] - GRID VALUE
  ;[bp + 6] [3RD PARAM] - GRID ROW
  ;[bp + 4] [4TH PARAM] - GRID COLUMN

  ;LOCAL VARIABLES: N/A

  push bp
  mov bp, sp
  push ax
  push bx

  ;FUNCTION START
  mov bx, [undo_stack_ptr]

  xor ax, ax
  mov ax, [bp + 10]
  mov [undo_stack + bx], al
  dec bx

  xor ax, ax
  mov ax, [bp + 8]
  mov [undo_stack + bx], al
  dec bx

  xor ax, ax
  mov ax, [bp + 6]
  mov [undo_stack + bx], al
  dec bx

  xor ax, ax
  mov ax, [bp + 4]
  mov [undo_stack + bx], al
  dec bx

  mov [undo_stack_ptr], bx

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop bx
  pop ax
  pop bp
ret 8

undo:
  ;FUNCTION NAME: UNDO

  ;PASSED PARAMETERS
  ;[bp + 4] [1ST PARAM] - TARGET GRID (POINTER)

  ;LOCAL VARIABLES: N/A

  push bp
  mov bp, sp
  push ax
  push bx
  push cx
  push dx
  push di
  push si

  ;FUNCTION START
  cmp word [undo_stack_ptr], 242
  jge end_undo

  mov bx, [undo_stack_ptr]

  xor ax, ax
  inc bx
  mov al, [undo_stack + bx]

  xor cx, cx
  inc bx
  mov cl, [undo_stack + bx]

  xor dx, dx
  inc bx
  mov dl, [undo_stack + bx]

  push 0
  push word [current_grid_ptr]
  push cx
  push ax
  call get_grid_value
  pop si

  push 1
  push si
  call update_number_card

  push word [bp + 4]
  push cx
  push ax
  push dx
  call set_grid_value

  xor ax, ax
  inc bx
  mov al, [undo_stack + bx]
  sub [score], ax

  mov [undo_stack_ptr], bx

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  end_undo:
  pop si
  pop di
  pop dx
  pop cx
  pop bx
  pop ax
  pop bp
ret 2

check_row_completion:
  ;FUNCTION NAME: CHECK_ROW_COMPLETION

  ;PASSED PARAMETERS
  ;[bp + 8] [1ST PARAM] - RETURN VALUE (0 - INCOMPLETE, 1 - COMPLETE)
  ;[bp + 6] [2ND PARAM] - GRID TO CHECK (POINTER)
  ;[bp + 4] [3RD PARAM] - ROW INDEX

  ;LOCAL VARIABLES: N/A

  push bp
  mov bp, sp
  push es
  push di
  push ax
  push cx

  ;FUNCTION START
  mov ax, ds
  mov es, ax
  
  mov ax, 9
  mul byte [bp + 4]
  mov di, ax
  add di, [bp + 6]

  xor ax, ax
  mov cx, 9

  check_loop:
    mov al, [es:di]
    cmp al, 0
    je exit_check_loop
    inc di
  loop check_loop

  exit_check_loop:
  cmp cx, 0
  jg else_crc
  mov word [bp + 8], 1
  jmp end_crc
  else_crc:
  mov word [bp + 8], 0

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  end_crc:
  pop cx
  pop ax
  pop di
  pop es
  pop bp
ret 4

check_column_completion:

  ;FUNCTION NAME: CHECK_COLUMN_COMPLETION

  ;PASSED PARAMETERS
  ;[bp + 8] [1ST PARAM] - RETURN VALUE (0 - INCOMPLETE, 1 - COMPLETE)
  ;[bp + 6] [2ND PARAM] - GRID TO CHECK (POINTER)
  ;[bp + 4] [3RD PARAM] - COLUMN INDEX

  ;LOCAL VARIABLES: N/A

  push bp
  mov bp, sp
  push es
  push di
  push ax
  push cx

  ;FUNCTION START
  mov ax, ds
  mov es, ax
  
  mov di, [bp + 4]
  add di, [bp + 6]

  xor ax, ax
  mov cx, 10
  num_check_ccc:
    mov al, [es:di]
    cmp al, 0
    je exit_num_check_ccc
    add di, 9 
  loop num_check_ccc
  
  exit_num_check_ccc:
  cmp cx, 0
  jg else_ccc
  mov word [bp + 8], 1
  jmp end_ccc
  else_ccc:
  mov word [bp + 8], 0

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  end_ccc:
  pop cx
  pop ax
  pop di
  pop es
  pop bp
ret 4

add_score:
  ;FUNCTION NAME: ADD_SCORE

  ;PASSED PARAMETERS:
  ;[bp + 8] [1ST PARAM] - CURRENT GRID (POINTER)
  ;[bp + 6] [2ND PARAM] - CURSOR ROW
  ;[bp + 4] [3RD PARAM] - CURSOR COLUMN

  ;LOCAL VARIABLES: N/A

  push bp
  mov bp, sp
  push ax
  push dx

  ;FUNCTION START
  xor dx, dx
  mov ax, 10
  
  push 0
  push word [bp + 8]
  push word [bp + 6]
  call check_row_completion
  pop dx

  cmp dx, 0
  je else_as
  add ax, 10

  else_as:
  push 0
  push word [bp + 8]
  push word [bp + 4]
  call check_column_completion
  pop dx

  cmp dx, 1
  jne else_as_2
  add ax, 10

  else_as_2:
  cmp word [minutes], 5
  jb multiply_score
  add ax, 5

  multiply_score:
  mov dx, [score_multiplier]
  mul byte dl
  add [score], ax
  mov [temp_value], ax

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop dx
  pop ax
  pop bp
ret 6

update_number_card:
  ;FUNCTION NAME: UPDATE_NUMBER_CARD

  ;PASSED PARAMETERS
  ;[bp + 6] [1ST PARAM] - MODE (0 - DECREMENT, 1 - INCREMENT)
  ;[bp + 4] [2ND PARAM] - NUMBER CARD INDEX

  ;LOCAL VARIABLES: N/A

  push bp
  mov bp, sp
  push si

  ;FUNCTION START
  mov si, [bp + 4]
  sub si, 1
  shl si, 1

  cmp word [bp + 6], 0
  jne else_unc
  dec word [number_cards + si]
  jmp exit_unc

  else_unc:
  inc word [number_cards + si]

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  exit_unc:
  pop si
  pop bp
ret 4

set_number_cards:
  ;FUNCTION NAME: SET_NUMBER_CARDS
  ;PASSED PARAMETERS: N/A
  ;LOCAL VARIABLES: N/A

  push bp
  mov bp, sp
  push ax
  push bx
  push cx
  push ds
  push si
  
  ;FUNCTION START
  xor ax, ax
  mov si, [current_grid_ptr]
  mov cx, 81

  cld
  loop_snc:
    lodsb
    cmp ax, 0
    jle end_loop_snc
    cmp ax, 9
    jg end_loop_snc
    mov bx, ax
    dec bx
    shl bx, 1
    dec word [number_cards + bx]
  end_loop_snc:
  loop loop_snc

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop si
  pop ds
  pop cx
  pop bx
  pop ax
  pop bp
ret
  
reset_number_cards:
  ;FUNCTION NAME: RESET_NUMBER_CARDS
  ;PASSED PARAMETERS: N/A
  ;LOCAL VARIABLES: N/A

  push bp
  mov bp, sp
  push ax
  push cx
  push es
  push di

  ;FUNCTION START
  push ds
  pop es
  mov di, number_cards
  mov cx, 9
  mov ax, 9

  cld
  rep stosw

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop di
  pop es
  pop cx
  pop ax
  pop bp
ret

draw_inside_grid:
  ;FUNCTION NAME: DRAW_INSIDE_GRID

  ;PASSED PARAMETERS
  ;[bp + 8] [1ST PARAM] - CHARACTER TO PRINT (ASCII)
  ;[bp + 6] [2ND PARAM] - GRID ROW
  ;[bp + 4] [3RD PARAM] - GRID COLUMN

  ;LOCAL VARIABLES
  ;[bp - 2] - TEMP CURSOR ROW
  ;[bp - 4] - TEMP CURSOR COLUMN
  
  push bp
  mov bp, sp
  sub sp, 4
  push es
  push di
  push ax
  push bx
  push cx
  push dx

  ;FUNCTION START
  mov ax, [bp + 6]
  mov [bp - 2], ax

  cmp word [bp + 6], 6
  jb else_dc_3
  sub word [bp - 2], 6

  else_dc_3:
  mov ax, [bp + 4]
  mov [bp - 4], ax

  mov ax, 4
  mul byte [bp - 2]
  mov cx, ax

  mov ax, 6
  mul byte [bp - 4]
  mov dx, ax

  cmp word [bp - 4], 3
  jl else_dc_1
  inc dx

  else_dc_1:
  cmp word [bp - 4], 6
  jl else_dc_2
  inc dx

  else_dc_2:
  push ds
  pop es
  mov ax, 80
  mov bl, 2
  add bl, cl
  mul byte bl
  add ax, 4
  add ax, dx
  shl ax, 1
  mov di, ax
  add di, off_screen_buffer

  mov ax, [bp + 8]
  mov [es:di], ax

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop dx
  pop cx
  pop bx
  pop ax
  pop di
  pop es
  add sp, 4
  pop bp
ret 6

reset_cursor_position:
  ;FUNCTION NAME: RESET_CURSOR_POSITION
  ;PASSED PARAMETERS: N/A
  ;LOCAL VARIABLES: N/A

  cmp word [scroll_flag], 1
  jne else_rsp
  mov word [cursor_row], 0
  mov word [cursor_col], 0
  jmp exit_rsp

  else_rsp:
    mov word [cursor_row], 6
    mov word [cursor_col], 0

  exit_rsp:
ret

draw_grid:
  ;FUNCTION NAME: DRAW_GRID

  ;PASSED PARAMETERS:
  ;[bp + 4] [1ST PARAM] - GRID TO DRAW (POINTER)

  ;LOCAL VARIABLES: N/A

  push bp
  mov bp, sp
  pusha

  ;FUNCTION START
  mov si, [bp + 4]
  mov cx, 0
  mov dx, 0

  cmp word [scroll_flag], 1
  jne draw_lower_half
  
  upper_half_ol:
    
    cmp byte [si], 0
    je print_notes_uh

    mov ax, main_theme
    mov al, [si]
    add al, '0'
    push ax
    push cx
    push dx
    call draw_inside_grid
    jmp skip_printing_uh

    print_notes_uh:
    push main_notes_theme
    push cx
    push dx
    call print_notes_grid

    skip_printing_uh:
    inc si
    inc dx
    cmp dx, 9
    jne skip_reset
    mov dx, 0
    inc cx
    skip_reset:
    cmp cx, 5
    jbe upper_half_ol
  jmp exit_dg

  draw_lower_half:
      add si, 54
      mov cx, 6

    lower_half_ol:
      cmp byte [si], 0
      je print_notes_lh

      mov ax, main_theme
      mov al, [si]
      add al, '0'
      push ax
      push cx
      push dx
      call draw_inside_grid
      jmp skip_printing_lh

      print_notes_lh:
      push main_theme
      push cx
      push dx
      call print_notes_grid
  
      skip_printing_lh:
      inc si
      inc dx
      cmp dx, 9
      jne skip_reset_lh
      mov dx, 0
      inc cx
      skip_reset_lh:
      cmp cx, 8
    jbe lower_half_ol

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  exit_dg:
  popa
  pop bp
  ret 2

get_grid_value:
  ;FUNCTION NAME: GET_GRID_VALUE

  ;PASSED PARAMETERS
  ;[bp + 10] [1ST PARAM] - RETURN VALUE
  ;[bp + 8]  [2ND PARAM] - GRID TO READ (POINTER)
  ;[bp + 6]  [3RD PARAM] - ROW OF THE GRID
  ;[bp + 4]  [4TH PARAM] - COLUMN OF THE GRID

  ;LOCAL VARIABLES: N/A

  push bp 
  mov bp, sp
  pusha

  ;FUNCTION START
  mov ax, 9
  mul byte [bp + 6]
  add word ax, [bp + 4]
  mov si, ax
  add si, [bp + 8]

  xor ax, ax
  mov al, [si]
  mov [bp + 10], ax

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  popa
  pop bp
ret 6

set_grid_value:
  ;FUNCTION NAME: SET_GRID_VALUE

  ;PASSED PARAMETERS
  ;[bp + 10]  [1ST PARAM] - GRID TO WRITE (POINTER)
  ;[bp + 8]  [2ND PARAM] - ROW OF THE GRID
  ;[bp + 6]  [3RD PARAM] - COLUMN OF THE GRID
  ;[bp + 4] [4TH PARAM] - VALUE TO SET

  ;LOCAL VARIABLES: N/A

  push bp
  mov bp, sp
  push ax
  push si

  ;FUNCTION START
  mov ax, 9
  mul byte [bp + 8]
  add word ax, [bp + 6]
  mov si, ax
  add si, [bp + 10]

  mov ax, [bp + 4]
  mov [si], al

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop si
  pop ax
  pop bp
ret 8

copy_grid:
  ;FUNCTION NAME: COPY_GRID

  ;PASSED PARAMETERS
  ;[bp + 6] [1ST PARAM] - SOURCE GRID (POINTER)
  ;[bp + 4] [2ND PARAM] - DESTINATION GRID (POINTER)

  ;LOCAL VARIABLES: N/A

  push bp
  mov bp, sp
  push ds
  push si
  push es
  push di
  push cx

  ;FUCTION START
  push ds
  pop es
  mov si, [bp + 6]
  mov di, [bp + 4]
  mov cx, 81
  rep movsb

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop cx
  pop di
  pop es
  pop si
  pop ds
  pop bp
ret 4

draw_line:
  ;FUNCTION NAME: DRAW_LINE

  ;PASSED PARAMETERS
  ;[bp + 12] [1ST PARAM] - VERTICAL MODE (160) OR HORIZONTAL MODE (2)
  ;[bp + 10] [2ND PARAM] - LENGTH OF THE LINE
  ;[bp + 8]  [3RD PARAM] - FORMAT OF THE LINE
  ;[bp + 6]  [4TH PARAM] - X-COORDINATE OF THE LINE
  ;[bp + 4]  [5TH PARAM] - Y-COORDINATE OF THE LINE

  ;LOCAL VARIABLES
  ;N/A

  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push bp
  mov  bp, sp
  push ax
  push cx
  push es
  push di

  ;FUNCTION START
  mov  ax, ds
  mov  es, ax

  mov  ax, 80
  mul  byte [bp + 4]
  add ax, [bp + 6]  
  shl ax, 1
  mov  di, ax     
  add di, off_screen_buffer

  mov  cx, [bp + 10] 
  mov  ax, [bp + 8] 

  l_print_loop:
    mov [es:di], ax
    add di, [bp + 12]
  loop l_print_loop

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
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
  cmp word [bp + 12], 0
  je text_mode

  number_mode:

  mov si, [bp + 10]
  mov ax, [si]
  xor cx, cx
  xor dx, dx

  stack_push_loop:

    div word [bp - 2]
    add dx, '0'
    push dx
    xor dx, dx
    inc cx
    cmp ax, 0
  jne stack_push_loop

  mov ax, sp
  mov dx, cx

  number_print_loop:
    
    push word mode_text
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
  jmp print_text_end

  text_mode:
    mov  ax, ds
    mov  es, ax

    mov ax, 80
    mul byte [bp + 4] 
    add ax, [bp + 6]   
    shl ax, 1
    mov  di, ax        
    add  di, off_screen_buffer

    mov  si, [bp + 10] 
    mov  ax, [bp + 8] 
    
    text_print_loop:
      mov al, [si]
      stosw
      inc si
      cmp byte [si], 0
  jne text_print_loop

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
  ;FUNCTION NAME: MOVE_BUFFER_TO_SCREEN (MOVE BUFFER TO SCREEN)

  ;PASSED PARAMETERS
  ;N/A

  ;LOCAL VARIABLES
  ;N/A

  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push ds
  push si
  push es
  push di
  push ax
  push cx

  ;FUNCTION START
  mov si, off_screen_buffer
  mov ax, 0xb800
  mov es, ax
  mov di, 0
  mov cx, 2000

  cld
  rep movsw

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop cx
  pop ax
  pop di
  pop es
  pop si
  pop ds
ret

draw_bg_to_buffer: 
  ;FUNCTION NAME: DRAW_BG_TO_BUFFER (DRAW BACKGROUND TO BUFFER)

  ;PASSED PARAMETERS
  ;[bp + 6][1ST PARAM] - FORMAT OF THE BACKGROUND
  ;[bp + 4][2ND PARAM] - COLOR OF THE BACKGROUND

  ;LOCAL VARIABLES
  ;N/A

  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES  
  push bp
  mov  bp, sp

  push es
  push ax
  push di
  push si
  push cx

  ;FUNCTION START
  mov ax, ds
  mov es, ax
  mov di, off_screen_buffer

  mov ax, [bp + 6]
  mov cx, 2000

  cld 
  rep stosw

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop cx
  pop si
  pop di
  pop ax
  pop es
  pop bp
ret 4

draw_bg:   
  ;FUNCTION NAME: DRAW_BG (DRAW BACKGROUND)

  ;PASSED PARAMETERS
  ;[bp + 6][1ST PARAM] - FORMAT OF THE BACKGROUND
  ;[bp + 4][2ND PARAM] - COLOR OF THE BACKGROUND

  ;LOCAL VARIABLES
  ;N/A

  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push bp
  mov  bp, sp

  push es
  push ax
  push di
  push si
  push cx

  ;FUNCTION START
  mov ax, 0xb800
  mov es, ax
  mov di, 0

  mov ax, [bp + 6]
  mov cx, 2000

  cld 
  rep stosw

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop cx
  pop si
  pop di
  pop ax
  pop es
  pop bp
ret 4

draw_mm:
  ;FUNCTION NAME: DRAW_MM (DRAW MAIN MENU)

  ;PASSED PARAMETERS
  ;N/A

  ;LOCAL VARIABLES
  ;N/A

  ;PARAMETERS FOR PRINT_TEXT FUNCTION
  ;[bp + 12][1ST PARAM] - STRING MODE (0) OR NUMBER MODE (1)
  ;[bp + 10][2ND PARAM] - TEXT TO PRINT
  ;[bp + 8] [3RD PARAM] - TEXT FORMAT
  ;[bp + 6] [4TH PARAM] - X-COORDINATE OF STARTING POINT
  ;[bp + 4] [5TH PARAM] - Y-COORDINATE OF STARTING POINT

  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push bp
  mov  bp, sp

  push ax
  push es

  ;FUNCTION START
  mov ax, 0xb800
  mov es, ax

  mov ax, main_theme

  push 2
  push 8
  mov al, horizontal_subline_char
  push ax
  push 37
  push 10
  call draw_line

  push 2
  push 8
  mov al, horizontal_subline_char
  push ax
  push 37
  push 15
  call draw_line

  push 2
  push 8
  mov al, horizontal_subline_char
  push ax
  push 37
  push 12
  call draw_line

  push 160
  push 6
  mov al, horizontal_subline_char
  push ax
  push 36
  push 10
  call draw_line

  push 160
  push 6
  mov al, horizontal_subline_char
  push ax
  push 45
  push 10
  call draw_line

  mov ax, main_theme


  push word mode_text
  push sudoku
  push main_theme
  push word 38
  push word 11
  call print_text

  cmp word [gen_flag], 0
  jne skip_pgf

  mov ax, current_selection_theme
  
  skip_pgf:
  
  push word mode_text
  push play_game
  push ax
  push word 39
  push word 13
  call print_text

  mov ax, main_theme

  cmp word [gen_flag], 1
  jne skip_ef

  mov ax, current_selection_theme
  
  skip_ef:

  push word mode_text
  push exit_s
  push ax
  push word 39
  push word 14
  call print_text

  call move_buffer_to_screen

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop es
  pop ax
  pop bp
ret

draw_ls:
  ;FUNCTION NAME: DRAW_LS (DRAW LEVEL SELECT)

  ;PASSED PARAMETERS
  ;N/A

  ;LOCAL VARIABLES
  ;N/A

  ;PARAMETERS FOR PRINT_TEXT FUNCTION
  ;[bp + 12][1ST PARAM] - STRING MODE (0) OR NUMBER MODE (1)
  ;[bp + 10][2ND PARAM] - TEXT TO PRINT
  ;[bp + 8] [3RD PARAM] - TEXT FORMAT
  ;[bp + 6] [4TH PARAM] - X-COORDINATE OF STARTING POINT
  ;[bp + 4] [5TH PARAM] - Y-COORDINATE OF STARTING POINT

  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push bp
  mov  bp, sp

  push ax
  push es

  ;FUNCTION START
  mov ax, 0xb800
  mov es, ax

  mov ax, main_theme

  push word mode_text
  push level_select
  push word main_theme
  push word 34
  push word 10
  call print_text

  cmp word [gen_flag], 0
  jne skip_lsf
  mov ax, current_selection_theme
  mov word [difficulty], hard
  push hard_grid
  push curr_grid
  call copy_grid
  
  mov word [current_grid_ptr], curr_grid
  mov word [solution_grid_ptr], hard_grid_sol
  mov word [mistakes_left], 1
  mov word [seconds], 0
  mov word [minutes], 6
  mov word [score_multiplier], 3
  
  skip_lsf:
  push word mode_text
  push easy
  push ax
  push word 38
  push word 12
  call print_text

  mov ax, main_theme

  cmp word [gen_flag], 1
  jne skip_esf

  mov ax, current_selection_theme
  mov word [difficulty], easy
  push easy_grid
  push curr_grid
  call copy_grid
  mov word [current_grid_ptr], curr_grid
  mov word [solution_grid_ptr], easy_grid_sol
  mov word [mistakes_left], 3
  mov word [seconds], 59
  mov word [minutes], 9
  mov word [score_multiplier], 1

  skip_esf:
  push word mode_text
  push medium
  push ax
  push word 37
  push word 13
  call print_text

  mov ax, main_theme

  cmp word [gen_flag], 2
  jne skip_hf

  mov ax, current_selection_theme
  mov word [difficulty], medium
  push medium_grid
  push curr_grid
  call copy_grid
  mov word [current_grid_ptr], curr_grid
  mov word [solution_grid_ptr], medium_grid_sol
  mov word [mistakes_left], 2
  mov word [seconds], 0
  mov word [minutes], 8
  mov word [score_multiplier], 2
  
  skip_hf:
  push word mode_text
  push hard
  push ax
  push word 38
  push word 14
  call print_text

  call move_buffer_to_screen

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
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
  push es
  
  mov word [bp - 2], 37
  mov word [bp - 4], 10
  mov word [bp - 6], main_theme
  mov word [bp - 8], current_selection_theme
  
  ;FUNCTION START
  push word mode_text
  push word game_over_s
  push word [bp - 6]
  push word [bp - 2]
  push word [bp - 4]
  call print_text

  add word [bp - 4], 2
  sub word [bp - 2], 5

  push word mode_text
  push word time_elapsed_s
  push word [bp - 6]
  push word [bp - 2]
  push word [bp - 4]
  call print_text

  add word [bp - 2], 16

  push word 1
  push word minutes
  push word [bp - 6]
  push word [bp - 2]
  push word [bp - 4]
  call print_text

  add word [bp - 2], 1

  push word 0
  push word colon
  push word [bp - 6]
  push word [bp - 2]
  push word [bp - 4]
  call print_text

  add word [bp - 2], 1

  push word 1
  push word seconds
  push word [bp - 6]
  push word [bp - 2]
  push word [bp - 4]
  call print_text

  add word [bp - 4], 1
  sub word [bp - 2], 13

  push word 0
  push word score_s
  push word [bp - 6]
  push word [bp - 2]
  push word [bp - 4]
  call print_text

  add word [bp - 2], 7

  push word 1
  push word score
  push word [bp - 6]
  push word [bp - 2]
  push word [bp - 4]
  call print_text

  call move_buffer_to_screen
  
  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop es
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

  mov word [bp - 2], 5
  mov word [bp - 4], 5
  
  ;FUNCTION START
  push word mode_text
  push difficulty_s ;PRINTING DIFFICULTY STAT
  push word [bp + 8]
  mov dx, [bp + 6]
  add dx, [bp - 4]
  push dx
  push word [bp - 2]
  call print_text

  add word [bp - 2], 1

  push word mode_text
  push word [difficulty]
  push word [bp + 8]
  mov dx, [bp + 6]
  add dx, [bp - 4]
  push dx
  push word [bp - 2]
  call print_text

  add word [bp - 2], 2

  push word mode_text
  push timer_s       ;PRINTING TIMER STAT
  push word [bp + 8]
  mov dx, [bp + 6]
  add dx, [bp - 4]
  push dx
  push word [bp - 2]
  call print_text

  add word [bp - 2], 1

  mov dx, [bp + 6]
  add dx, [bp - 4]
  push dx
  push word [bp - 2]
  call draw_timer

  add word [bp - 2], 2

  push word mode_text
  push score_s       ;PRINTING SCORE STAT
  push word [bp + 8]
  mov dx, [bp + 6]
  add dx, [bp - 4]
  push dx
  push word [bp - 2]
  call print_text

  add word [bp - 2], 1

  push word mode_number
  push score
  push word [bp + 8]
  mov dx, [bp + 6]
  add dx, [bp - 4]
  push dx
  push word [bp - 2]
  call print_text

  add word [bp - 2], 2

  push word mode_text
  push mistakes_s ;PRINTING MISTAKES STAT
  push word [bp + 8]
  mov dx, [bp + 6]
  add dx, [bp - 4]
  push dx
  push word [bp - 2]
  call print_text

  add word [bp - 2], 1

  push word mode_number
  push mistakes_left
  push word [bp + 8]
  mov dx, [bp + 6]
  add dx, [bp - 4]
  push dx
  push word [bp - 2]
  call print_text

  add word [bp - 2], 2

  push word mode_text
  push notes_s       ;PRINTING NOTES STAT
  push word [bp + 8]
  mov dx, [bp + 6]
  add dx, [bp - 4]
  push dx
  push word [bp - 2]
  call print_text

  add word [bp - 2], 1

  push word mode_text
  push word [current_notes_setting]
  push word [notes_s_theme]
  mov dx, [bp + 6]
  add dx, [bp - 4]
  push dx
  push word [bp - 2]
  call print_text

  add word [bp - 2], 2

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop dx
  pop cx
  pop bx
  pop ax
  add sp, 4
  pop bp
ret 6

draw_ncs:
  ;NAME: DRAW_NCS (DRAW NUMBER CARD SCREEN)

  ;PASSED PARAMETERS
  ;[bp + 8] - FORMAT 
  ;[bp + 6] - X-COORDINATE OF VERTICAL DIVIDER LINE 
  ;[bp + 4] - Y-COORDINATE OF HORIZONTAL DIVIDER LINE

  ;LOCAL VARIABLES
  ;[bp - 2] - ROW INDEX OF NEXT AVAILABLE LINE
  ;[bp - 4] - OFFSET FROM STARTING POINT

  ;PASSED PARAMETERS
  ;[bp + 12][1ST PARAM] - STRING MODE (0) OR NUMBER MODE (1)
  ;[bp + 10][2ND PARAM] - TEXT TO PRINT
  ;[bp + 8] [3RD PARAM] - TEXT FORMAT
  ;[bp + 6] [4TH PARAM] - X-COORDINATE OF STARTING POINT
  ;[bp + 4] [5TH PARAM] - Y-COORDINATE OF STARTING POINT
   
  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push bp
  mov bp, sp
  sub sp, 4
  push ax
  push bx
  push cx
  push dx

  mov word [bp - 2], 3
  mov word [bp - 4], 7
  
  ;FUNCTION START

  mov word [curr_index], 0 
  mov cx, 9

  ncs_print_loop:
    mov ax, [bp + 6]
    add ax, [bp - 4]
    inc word [curr_index]

    push 1
    push word curr_index
    push main_theme
    push ax
    push word [bp - 2]
    call print_text

    add ax, 2

    push 0
    push word pipe
    push main_theme
    push ax
    push word [bp - 2]
    call print_text

    add ax, 2
    mov bx, [curr_index]
    shl bx, 1
    add bx, number_cards
    sub bx, 2

    push 1
    push bx
    push highlight_theme
    push ax
    push word [bp - 2]
    call print_text

    add word [bp - 2], 2
  loop ncs_print_loop

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
  mov word [bp - 10], line_theme
  mov word [bp - 12], main_theme
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
      mov  dl, horizontal_subline_char
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
      mov  dl, vertical_subline_char
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

  push word [current_grid_ptr]
  call draw_grid

  cmp word [notes_flag], 1
  je skip_drawing_cursor

  push 0
  push word [current_grid_ptr]
  push word [cursor_row]
  push word [cursor_col]
  call get_grid_value
  pop ax
  mov ah, cursor_sel_attr_byte;
  add al, '0'
  cmp al, '0'
  jne cell_occupied
  mov al, cursor_icon
  cell_occupied:
  push ax
  push word [cursor_row]
  push word [cursor_col]
  call draw_inside_grid

  skip_drawing_cursor:
  
  push word [bp - 12]
  push word [bp - 6] 
  push word [bp - 8]

  cmp word [ncs_flag], 0
  jne skip_gs_te_call

  call draw_gs_te
  jmp notes_decision

  skip_gs_te_call:
  call draw_ncs

  notes_decision:
  cmp word [notes_flag], 0
  je move_gs_to_screen
  call draw_note_elements
  
  move_gs_to_screen:
  call move_buffer_to_screen

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop dx
  pop cx
  pop ax
  add sp, 18
  pop bp
ret 2

draw_note_elements:
  push bp
  mov bp, sp

  push notes_background
  push word [cursor_row]
  push word [cursor_col]
  call change_notes_grid_bg

  pop bp
ret

refresh_side_screen:
  ;NAME: REFRESH_SIDE_SCREEN (REFRESH SIDE SCREEN)

  ;PASSED PARAMETERS
  ;N/A

  ;LOCAL VARIABLES
  ;[bp - 2]  - X-COORDINATE OF STARTING POINT
  ;[bp - 4]  - LENGTH OF THE LINE

  ;PARAMETERS FOR DRAW_LINE FUNCTION
  ;[bp + 12] [1ST PARAM] - VERTICAL MODE (160) OR HORIZONTAL MODE (2)
  ;[bp + 10] [2ND PARAM] - LENGTH OF THE LINE
  ;[bp + 8]  [3RD PARAM] - FORMAT OF THE LINE
  ;[bp + 6]  [4TH PARAM] - X-COORDINATE OF THE LINE
  ;[bp + 4]  [5TH PARAM] - Y-COORDINATE OF THE LINE

  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push bp
  mov bp, sp
  sub sp, 4
  mov word [bp - 2], 63
  mov word [bp - 4], 25

  ;FUNCTION START
  refresh_side_screen_loop:
  push 160
  push word [bp - 4]
  push main_theme
  push word [bp - 2]
  push 0
  call draw_line

  inc word [bp - 2]
  cmp word [bp - 2], 80
  jb refresh_side_screen_loop

  call move_buffer_to_screen
  
  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  add sp, 4
  pop bp
ret

clear_screen:
  ;FUNCTION NAME: CLEAR_SCREEN (CLEAR SCREEN)

  ;PASSED PARAMETERS
  ;N/A

  ;LOCAL VARIABLES
  ;N/A

  ;PARAMETERS FOR DRAW_BG FUNCTION
  ;[bp + 6][1ST PARAM] - FORMAT OF THE BACKGROUND
  ;[bp + 4][2ND PARAM] - COLOR OF THE BACKGROUND

  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push bp
  mov bp, sp

  ;FUNCTION START
  push clear_screen_theme
  push 01h
  call draw_bg

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop bp
ret

refresh_buffer:
  ;FUNCTION NAME: REFRESH_BUFFER (REFRESH BUFFER)

  ;PASSED PARAMETERS
  ;N/A

  ;LOCAL VARIABLES
  ;N/A

  ;PARAMETERS FOR DRAW_BG_TO_BUFFER FUNCTION
  ;[bp + 6][1ST PARAM] - FORMAT OF THE BACKGROUND
  ;[bp + 4][2ND PARAM] - COLOR OF THE BACKGROUND

  push bp
  mov bp, sp

  ;FUNCTION START
  push main_theme
  push 01h
  call draw_bg_to_buffer
  
  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop bp
ret

clear_buffer:
  ;FUNCTION NAME: CLEAR_BUFFER (CLEAR BUFFER)

  ;PASSED PARAMETERS
  ;N/A

  ;LOCAL VARIABLES
  ;N/A

  ;PARAMETERS FOR DRAW_BG_TO_BUFFER FUNCTION
  ;[bp + 6][1ST PARAM] - FORMAT OF THE BACKGROUND
  ;[bp + 4][2ND PARAM] - COLOR OF THE BACKGROUND

  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push bp
  mov bp, sp

  ;FUNCTION START
  push clear_screen_theme
  push 01h
  call draw_bg_to_buffer

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop bp
ret

draw_timer:
  ;FUNCTION NAME: DRAW_TIMER (DRAW TIMER)

  ;PASSED PARAMETERS
  ;[bp + 6][1ST PARAM] - X-COORDINATE OF STARTING POINT
  ;[bp + 4][2ND PARAM] - Y-COORDINATE OF STARTING POINT

  ;LOCAL VARIABLES
  ;N/A

  ;PARAMETERS FOR PRINT_TEXT FUNCTION
  ;[bp + 12][1ST PARAM] - STRING MODE (0) OR NUMBER MODE (1)
  ;[bp + 10][2ND PARAM] - TEXT TO PRINT
  ;[bp + 8] [3RD PARAM] - TEXT FORMAT
  ;[bp + 6] [4TH PARAM] - X-COORDINATE OF STARTING POINT
  ;[bp + 4] [5TH PARAM] - Y-COORDINATE OF STARTING POINT

  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push bp
  mov bp, sp

  ;FUNCTION START
  mov cx, [bp + 6]
  add cx, 3

  cmp word [seconds], 9
  jg skip

  push 1
  push word zero
  push main_theme
  push cx
  push word [bp + 4]
  call print_text
  inc cx
  
  skip:
  push 1
  push word minutes
  push main_theme
  push word [bp + 6]
  push word [bp + 4]
  call print_text

  add word [bp + 6], 2

  push 0
  push word colon
  push main_theme
  push word [bp + 6]
  push word [bp + 4]
  call print_text
  
  push 1
  push word seconds
  push main_theme
  push cx
  push word [bp + 4]
  call print_text

  call move_buffer_to_screen

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop bp
ret 4

timer_isr:
  ;FUNCTION NAME: TIMER_ISR (TIMER INTERRUPT SERVICE ROUTINE)

  ;PASSED PARAMETERS
  ;N/A

  ;LOCAL VARIABLES
  ;N/A

  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push bp
  mov bp, sp
  
  ;FUNCTION START
  inc word [tickcount]
  cmp word [tickcount], 18
  jne skip_drawing_timer

  dec word [seconds]
  mov word [tickcount], 0

  cmp word [seconds], 0
  jg call_draw_timer

  mov word [seconds], 59
  dec word [minutes]

  call_draw_timer:
    cmp word [ncs_flag], 0
    jne skip_drawing_timer

    push word 66
    push word 9
    call draw_timer
  
  ;FUNCTION END - SIGNALING THE END OF THE INTERRUPT SERVICE ROUTINE
  skip_drawing_timer:
    mov  al, 0x20
    out  0x20, al
  pop bp
iret

gs_kbisr:
  ;FUNCTION NAME: GS_KBISR (GAMESPACE KEYBOARD INTERRUPT SERVICE ROUTINE)
  ;PASSED PARAMETERS: N/A
  ;LOCAL VARIABLES: N/A

  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push bp
  mov bp, sp
  push ax
  push es
  push dx

  ;FUNCTION START
  xor ax, ax
  xor dx, dx
  in al, 0x60

  cmp al, 0x11; W
  jne nextcmp
  cmp word [cursor_row], 0
  jle exit_gs_kbisr
  dec word [cursor_row]
  cmp word [cursor_row], 5
  jne draw_gs_kbisr
  xor word [scroll_flag], 1
  jmp draw_gs_kbisr

nextcmp:
  cmp al, 0x1E; A
  jne nextcmp_2
  cmp word [cursor_col], 0
  jle exit_gs_kbisr
  dec word [cursor_col]
  jmp draw_gs_kbisr

nextcmp_2:
  cmp al, 0x1F ; S
  jne nextcmp_3
  cmp word [cursor_row], 8
  jge exit_gs_kbisr
  inc word [cursor_row]
  cmp word [cursor_row], 6
  jne draw_gs_kbisr
  xor word [scroll_flag], 1
  jmp draw_gs_kbisr

nextcmp_3:
  cmp al, 0x20; D
  jne nextcmp_4
  cmp word [cursor_col], 8
  jge exit_gs_kbisr
  inc word [cursor_col]
  jmp draw_gs_kbisr

nextcmp_4:
  cmp al, 22; U
  jne nextcmp_5
  push word [current_grid_ptr]
  call undo
  jmp draw_gs_kbisr

nextcmp_5:
  cmp al, 49; N
  jne nextcmp_6

  push 0
  push word [current_grid_ptr]
  push word [cursor_row]
  push word [cursor_col]
  call get_grid_value
  pop dx
  cmp dx, 0
  jne exit_gs_kbisr

  push es
  push 0
  pop es
  cli
  mov word [es:9 * 4], notes_kbisr
  mov [es:9 * 4 + 2], cs
  sti
  pop es

  mov word [current_notes_setting], on_s
  mov word [notes_s_theme], highlight_theme
  mov word [notes_flag], 1
  jmp draw_gs_kbisr

nextcmp_6:
  cmp al, 0x02; 1
  jl nomatch
  cmp al, 0x0A; 9
  jg nomatch
  dec al
  push 0
  push word [current_grid_ptr]
  push word [cursor_row]
  push word [cursor_col]
  call get_grid_value
  pop dx
  cmp dx, 0
  jne exit_gs_kbisr

  push 0
  push word [solution_grid_ptr]
  push word [cursor_row]
  push word [cursor_col]
  call get_grid_value
  pop dx
  cmp dl, al
  jne penalize_incorrect_input

  push 0
  push word [current_grid_ptr]
  push word [cursor_row]
  push word [cursor_col]
  call get_grid_value
  pop dx

  push word [current_grid_ptr]
  push word [cursor_row]
  push word [cursor_col]
  push ax
  call set_grid_value

  push word [current_grid_ptr]
  push word [cursor_row]
  push word [cursor_col]
  call add_score

  push word [temp_value] ;SCORE ADDED FOR THE INPUT
  push dx
  push word [cursor_row]
  push word [cursor_col]
  call grow_undo_stack

  push 0
  push ax
  call update_number_card

  push 0
  push word [current_grid_ptr]
  call check_if_grid_complete
  pop dx
  cmp dx, 0
  je play_row_col_completion_sound
  mov word [terminate_flag], 1
  jmp nomatch

penalize_incorrect_input:
  dec word [mistakes_left]
  cmp word [mistakes_left], 1
  jge play_mistake_sound
  mov word [terminate_flag], 1
jmp nomatch

play_row_col_completion_sound:
  push 0
  push word [current_grid_ptr]
  push word [cursor_row]
  call check_row_completion
  pop dx

  cmp dx, 1
  je play_sound_prccs

  push 0
  push word [current_grid_ptr]
  push word [cursor_col]
  call check_column_completion
  pop dx

  cmp dx, 1
  je play_sound_prccs
  jmp play_correct_sound

  play_sound_prccs:
  push row_col_track
  push row_col_track_d
  push word [row_col_track_s]
  call play_track
jmp draw_gs_kbisr

play_correct_sound:
  push correct_input_track
  push correct_input_track_d
  push word [correct_input_track_s]
  call play_track
jmp draw_gs_kbisr

play_mistake_sound:
  push incorrect_input_track
  push incorrect_input_track_d
  push word [incorrect_input_track_s]
  call play_track
  jmp draw_gs_kbisr

nomatch:
  pop dx
  pop es
  pop ax
  pop bp
  jmp far [cs:oldkbisr]

;DRAW GAMESPACE
draw_gs_kbisr:
  call refresh_buffer
  inc word [scroll_flag]
  push word [scroll_flag]
  dec word [scroll_flag]
  call draw_gs

;EXIT
exit_gs_kbisr:
  mov al, 0x20
  out 0x20, al

  pop dx
  pop es
  pop ax
  pop bp
iret

notes_kbisr:
  ;FUNCTION NAME: NOTES_KBISR (GAMESPACE KEYBOARD INTERRUPT SERVICE ROUTINE)
  ;PASSED PARAMETERS: N/A
  ;LOCAL VARIABLES: N/A

  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push bp
  mov bp, sp
  push ax
  push es
  push dx

  ;FUNCTION START
  xor ax, ax
  xor dx, dx
  in al, 0x60

  cmp al, 0x48
  je transfer_control_to_gs_kbisr
  cmp al, 0x50
  je transfer_control_to_gs_kbisr

  cmp al, 49
  jne nextcmp_notes_kbisr

  transfer_control_to_gs_kbisr:
  push es
  push 0
  pop es
  cli
  mov word [es:9 * 4], gs_kbisr
  mov [es:9 * 4 + 2], cs
  sti
  pop es
  mov word [current_notes_setting], off_s
  mov word [notes_s_theme], main_theme
  mov word [notes_flag], 0
  jmp draw_gs_notes_isr

  nextcmp_notes_kbisr:
  cmp al, 0x02; 1
  jl nomatch_notes_kbisr
  cmp al, 0x0A; 9
  jg nomatch_notes_kbisr
  dec ax

  push ax
  push word [cursor_row]
  push word [cursor_col]
  call toggle_note

  jmp draw_gs_notes_isr
  
;IF NO MATCH, EXIT
nomatch_notes_kbisr:
  pop dx
  pop es
  pop ax
  pop bp
  jmp far [cs:oldkbisr]

draw_gs_notes_isr:
call refresh_buffer
inc word [scroll_flag]
push word [scroll_flag]
dec word [scroll_flag]
call draw_gs

;EXIT
exit_notes_kbisr:
  mov al, 0x20
  out 0x20, al

  pop dx
  pop es
  pop ax
  pop bp
iret

hook_timer_interrupt:
  ;FUNCTION NAME: HOOK_TIMER_INTERRUPT (HOOK TIMER INTERRUPT)

  ;PASSED PARAMETERS
  ;N/A

  ;LOCAL VARIABLES
  ;N/A

  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push bp
  mov bp, sp

  push ds
  push es
  push ax
  push bx

  mov ax, 3508h      
  int 21h
  mov word [cs:oldisr], bx  
  mov word [cs:oldisr+2], es 

  xor  ax, ax
  mov  es, ax

  cli
  mov word [es:8*4], timer_isr  
  mov [es:8*4+2], cs      
  sti

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop bx
  pop ax
  pop es
  pop ds
  pop bp
ret

unhook_timer_interrupt:
  ;FUNCTION NAME: UNHOOK_TIMER_INTERRUPT (UNHOOK TIMER INTERRUPT)
  
  ;PASSED PARAMETERS
  ;N/A

  ;LOCAL VARIABLES
  ;N/A

  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push bp
  mov bp, sp

  push ds
  push ax
  push dx

  xor ax, ax
  mov es, ax

  cli
  push ds
  mov ax, 2508h     
  mov dx, [cs:oldisr]   
  mov ds, [cs:oldisr+2]
  int 21h
  pop ds
  sti

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop dx
  pop ax
  pop ds
  pop bp
ret

clear_timer:
  ;FUNCTION NAME: CLEAR_TIMER (CLEAR TIMER)

  ;PASSED PARAMETERS
  ;N/A

  ;LOCAL VARIABLES
  ;N/A

  ;FUNCTION START
  mov word [tickcount], 0
  mov word [minutes], 0
  mov word [seconds], 0
ret

hook_gs_kbisr:

  ;FUNCTION NAME: HOOK_GS_KBISR (HOOK GAMESPACE KEYBOARD INTERRUPT SERVICE ROUTINE)
  ;PASSED PARAMETERS: N/A
  ;LOCAL VARIABLES: N/A

  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push ax
  push es

  ;FUNCTION START
  xor ax, ax
  mov es, ax

  mov ax, [es: 9 * 4]
  mov [oldkbisr], ax

  mov ax, [es:9 * 4 + 2]
  mov [oldkbisr + 2], ax

  cli 
  mov word [es:9 * 4], gs_kbisr
  mov [es:9 * 4 + 2], cs
  sti 

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop es
  pop ax
ret

unhook_gs_kbisr:

  ;FUNCTION NAME: UNHOOK_GS_KBISR (UNHOOK GAMESPACE KEYBOARD INTERRUPT SERVICE ROUTINE)
  ;PASSED PARAMETERS: N/A
  ;LOCAL VARIABLES: N/A

  ;SAVING REGISTERS AND INITIALIZING LOCAL VARIABLES
  push ax
  push es

  ;FUNCTION START
  xor ax, ax
  mov es, ax

  cli 
  mov ax, [oldkbisr]
  mov word [es:9 * 4], ax
  mov ax, [oldkbisr + 2]
  mov [es:9 * 4 + 2], ax
  sti 

  ;FUNCTION END - RESTORING REGISTERS AND COLLAPSING STACK
  pop es
  pop ax
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
    skip_drawing_ls:
      cmp ah, 0x01
      je  main_menu_ol
      cmp ah, 0x1c
      jne ls_il

  call hook_timer_interrupt
  call hook_gs_kbisr
  mov word [scroll_flag], 1
  call reset_number_cards
  call set_number_cards
  call reset_cursor_position
  mov word [terminate_flag], 0
  mov word [score], 0
  mov word [current_notes_setting], off_s
  call clear_notes

  call refresh_buffer
  push word 2
  call draw_gs

  game_loop:
    mov ah, 0x00
    int 16h                    
                               
    cmp ah, 0x01               
    jne skip_jumping_to_mm_game_loop   

    call unhook_gs_kbisr
    call unhook_timer_interrupt
    jmp main_menu_ol
    
    skip_jumping_to_mm_game_loop:
    cmp ah, 0x48
    je flip_scroll_flag

    cmp ah, 46
    je flip_ncs_flag

    cmp ah, 0x50
    jne enter_key_check

    flip_scroll_flag:
     xor word [scroll_flag], 1
     call reset_cursor_position
    jmp draw_gs_game_loop

    flip_ncs_flag:
      xor word [ncs_flag], 1
    jmp draw_gs_game_loop
    
    draw_gs_game_loop:
     call refresh_buffer
     inc word [scroll_flag]
     push word [scroll_flag]
     dec word [scroll_flag]
     call draw_gs

    enter_key_check:
    cmp word [terminate_flag], 1
  jne game_loop
  
  call unhook_timer_interrupt
  call unhook_gs_kbisr
  call refresh_buffer
  call draw_es

  push track
  push track_d
  push word [track_s]
  call play_track

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