global our_code_starts_here

our_code_starts_here:
  ;; anf0 = 20 + 30
  mov eax, 20
  add eax, 30
  mov [esp - 4], eax

  ;; anf0 + 40
  mov eax, [esp - 4]
  add eax, 40

  ;; check if condition is zero
  cmp eax, 0

  ;; if not, jump to the then branch
  jne label_35_true

  ;; 12
  mov eax, 12                   ; else

  ;; jump to the end of the if expression
  jmp label_35_done

label_35_true:
  ;; anf1 = add1(2)
  mov eax, 2                    ; then
  add eax, 1                    ; then
  mov [esp - 4], eax            ; then

  ;; anf2 = 1 + anf1
  mov eax, 1                    ; then
  add eax, [esp - 4]            ; then
  mov [esp - 8], eax            ; then

  ;; anf3 = 3 + 4
  mov eax, 3                    ; then
  add eax, 4                    ; then
  mov [esp - 12], eax           ; then

  ;; anf4 = sub1(anf3)
  mov eax, [esp - 12]           ; then
  sub eax, 1                    ; then
  mov [esp - 16], eax           ; then

  ;; anf5 = anf2 + anf4
  mov eax, [esp - 8]            ; then
  add eax, [esp - 16]           ; then
  mov [esp - 20], eax           ; then

  ;; anf5 + 10
  mov eax, [esp - 20]           ; then
  add eax, 10                   ; then

label_35_done:

  ret
