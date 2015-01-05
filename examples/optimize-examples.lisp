;;;;
;;;;	Examples of using declarations to optimize code.
;;;;

(defun foo1 (x) (+ x 1))
(disassemble 'foo1)
;Disassembling from address #x12485A0:
;#x0: 55             push    ebp                 
;#x1: 8BEC           mov     ebp,esp             
;#x3: 57             push    edi                 
;#x4: 83F901         cmp     ecx,00001h          
;#x7: 7406           je      000F                
;#x9: FF96B4100000   call    near dword ptr [esi+0000010B4h] 
;#xF: 8B4508         mov     eax,[ebp+00008h]    
;#x12: BA08000000     mov     edx,0008            
;#x17: A807           test    al,007h             
;#x19: 7506           jne     0021                
;#x1B: 03C2           add     eax,edx             
;#x1D: 7108           jno     0027                
;#x1F: 2BC2           sub     eax,edx             
;#x21: FF96DC170000   call    near dword ptr [esi+0000017DCh] 
;#x27: B901000000     mov     ecx,0001            
;#x2C: 8BE5           mov     esp,ebp             
;#x2E: 5D             pop     ebp                 
;#x2F: C3             ret                         

(defun foo2 (x) 
	(declare 
		(optimize (safety 0)(speed 3)) 
		(fixnum x))
	(the fixnum (+ x 1)))

(disassemble 'foo2)
;Disassembling from address #x126AB20:
;#x0: 55             push    ebp                 
;#x1: 8BEC           mov     ebp,esp             
;#x3: 57             push    edi                 
;#x4: 8B4508         mov     eax,[ebp+00008h]    
;#x7: BA08000000     mov     edx,0008            
;#xC: 03C2           add     eax,edx             
;#xE: B901000000     mov     ecx,0001            
;#x13: 8BE5           mov     esp,ebp             
;#x15: 5D             pop     ebp                 
;#x16: C3             ret                         
