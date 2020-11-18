section .text
global two_fer

two_fer:               ; two_fer(char *name(rdi), char *buffer(rsi))
    xor rax, rax       ; zerro buffer counter, just in case
    push rdi           ; save name arg for later
    mov rdi, onefor    ; load first string as arg
    call fill          ; call fill buffer on it

    pop rdx            ; load name
    cmp rdx, 0         ; *name == null?
    je  fill_you       ; yes: jump down

    mov rdi, rdx       ; no: load name as arg
    call fill          ; call fill buffer on name

cont:
    mov rdi, oneforme  ; load last string as arg
    call fill          ; call fill buffer on it
    ret                ; avoid flowing into the fill_you part by returning

fill_you:
    mov rdi, you       ; load 'you' as arg
    call fill          ; call fill buffer on it
    jmp cont           ; jump up to fill the last part


fill:                  ; uses rcx, dl; rdi supplied; rsi static
    xor rcx, rcx       ; counter = 0
    jmp cond           ; start conditionally
then:
    mov [rsi+rax], dl  ; load byte (offset rax) to static buffer
    inc rax            ; buffer++
    inc rcx            ; counter++
cond:
    mov dl, [rdi+rcx]  ; load byte (offset rcx) from arg
    cmp dl, 0          ; byte == null?
    jne then           ; no: jump up
else:
    mov [rsi+rax], dl  ; c strings must end in nul
    ret                ; yes: return



section .rodata

onefor:
    db 'One for ', 0
you:
    db 'you', 0
oneforme:
    db ', one for me.', 0
