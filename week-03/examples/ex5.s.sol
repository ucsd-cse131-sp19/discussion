global our_code_starts_here

our_code_starts_here:
  ;; x = 0
  mov eax, 0
  mov [esp - 4], eax

  ;; sub1(x)
  mov eax, [esp - 4]
  sub eax, 1

  ;; add1(sub1(x))
  add eax, 1

  ;; sub1(add1(sub1(x)))
  sub eax, 1

  ret

