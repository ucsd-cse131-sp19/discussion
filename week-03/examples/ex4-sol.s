section .text
global our_code_starts_here

our_code_starts_here:
  ;; x = 10
  mov eax, 10            
  mov [esp - 4], eax     

  ;; y = 10
  mov eax, 10            
  mov [esp - 8], eax     

  ;; anf0 = x + y
  mov eax, [esp - 4]     
  add eax, [esp - 8]     
  mov [esp - 12], eax    

  ;; anf1 = anf0 + 5
  mov eax, [esp - 12]    
  add eax, 5             
  mov [esp - 4], eax     

  ;; y = 20
  mov eax, 20            
  mov [esp - 8], eax     

  ;; anf2 = y - 5
  mov eax, [esp - 8]     
  sub eax, 5             
  mov [esp - 8], eax     

  ;; anf1 + anf2
  mov eax, [esp - 4]     
  add eax, [esp - 8]     

  ret                    
