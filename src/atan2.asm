; From https://www.msx.org/forum/msx-talk/development/8-bit-atan2
; By ARTRAG, based on https://codebase64.org/doku.php?id=base:8bit_atan2_8-bit_angle by Johan Forslöf (doynax)
; Modified by NovaSquirrel for reformatting, more comments, making it work on Game Boy, and some small code changes

SECTION "atan2", ROM0

MACRO neg
	cpl
	inc a
ENDM

; 8-bit atan2

; Calculate the angle, in a 256-degree circle.
; The trick is to use logarithmic division to get the y/x ratio and
; integrate the power function into the atan table. 

;	input
; 	B = x, C = y	in -128,127
;
;	output
;	A = angle		in 0-255

;      |
;  q1  |  q0
;------+-------
;  q3  |  q2
;      |

atan2_bc_xy::
	ld	hl, $8000      ; L = 0 so the signs can get shifted in
	                   ; H = $80; allows saving one cycle over two "add $80"s

	; Test C's sign bit
	ld	a, c
	add	a, h
	rl	l              ; y-

	; Test B's sign bit
	ld	a, b
	add	a, h
	rl	l              ; x-

atan2_bc_xy_sign_l::
	; Branch based on quadrant
	dec	l
	jr	z,q1
	dec	l
	jr	z,q2
	dec	l
	jr	z,q3

q0:
	ld	h, HIGH(log2_tab)
	ld	l, b
	
	ld	a, [hl]         ; 32*log2(x)
	ld	l, c
	
	sub	[hl]            ; 32*log2(x/y)
	
	jr	nc, :+          ; |x|>|y|
		neg             ; |x|<|y|    A = 32*log2(y/x)
	:		
	ld	l,a

	ld	h, HIGH(atan_tab)
	ld	a, [hl]
	ret	c              ; |x|<|y|
	
	neg
	and	$3F            ; |x|>|y|
	ret

q1:
	ld	a, b
	neg
	ld	b, a

	call q0
	neg
	and	$7F
	ret
		
q2:
	ld	a, c
	neg
	ld	c, a

	call q0
	neg
	ret		
		
q3:
	ld	a, b
	neg
	ld	b, a

	ld	a, c
	neg
	ld	c, a

	call q0
	add	128
	ret
		
SECTION "atan_tab", ROM0, ALIGN[8]
; z=0:255; sprintf('%d,',fix(atan(2.^(z/32))*128/pi))
;;;;;;;; atan(2^(x/32))*128/pi ;;;;;;;;
atan_tab:
	db $20,$20,$20,$21,$21,$22,$22,$23,$23,$23,$24,$24,$25,$25,$26,$26
	db $26,$27,$27,$28,$28,$28,$29,$29,$2A,$2A,$2A,$2B,$2B,$2C,$2C,$2C
	db $2D,$2D,$2D,$2E,$2E,$2E,$2F,$2F,$2F,$30,$30,$30,$31,$31,$31,$31
	db $32,$32,$32,$32,$33,$33,$33,$33,$34,$34,$34,$34,$35,$35,$35,$35
	db $36,$36,$36,$36,$36,$37,$37,$37,$37,$37,$37,$38,$38,$38,$38,$38
	db $38,$39,$39,$39,$39,$39,$39,$39,$39,$3A,$3A,$3A,$3A,$3A,$3A,$3A
	db $3A,$3B,$3B,$3B,$3B,$3B,$3B,$3B,$3B,$3B,$3B,$3B,$3C,$3C,$3C,$3C
	db $3C,$3C,$3C,$3C,$3C,$3C,$3C,$3C,$3C,$3D,$3D,$3D,$3D,$3D,$3D,$3D
	db $3D,$3D,$3D,$3D,$3D,$3D,$3D,$3D,$3D,$3D,$3D,$3D,$3E,$3E,$3E,$3E
	db $3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E
	db $3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3E,$3F,$3F,$3F,$3F
	db $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F
	db $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F
	db $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F
	db $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F
	db $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F
 
SECTION "log2_tab", ROM0, ALIGN[8]
; x=0:255;x(1)=1; sprintf('%d,',fix(32*log2(x)))
;;;;;;;; log2(x)*32 ;;;;;;;; 
log2_tab:
	db $00,$00,$20,$32,$40,$4A,$52,$59,$60,$65,$6A,$6E,$72,$76,$79,$7D
	db $80,$82,$85,$87,$8A,$8C,$8E,$90,$92,$94,$96,$98,$99,$9B,$9D,$9E
	db $A0,$A1,$A2,$A4,$A5,$A6,$A7,$A9,$AA,$AB,$AC,$AD,$AE,$AF,$B0,$B1
	db $B2,$B3,$B4,$B5,$B6,$B7,$B8,$B9,$B9,$BA,$BB,$BC,$BD,$BD,$BE,$BF
	db $C0,$C0,$C1,$C2,$C2,$C3,$C4,$C4,$C5,$C6,$C6,$C7,$C7,$C8,$C9,$C9
	db $CA,$CA,$CB,$CC,$CC,$CD,$CD,$CE,$CE,$CF,$CF,$D0,$D0,$D1,$D1,$D2
	db $D2,$D3,$D3,$D4,$D4,$D5,$D5,$D5,$D6,$D6,$D7,$D7,$D8,$D8,$D9,$D9
	db $D9,$DA,$DA,$DB,$DB,$DB,$DC,$DC,$DD,$DD,$DD,$DE,$DE,$DE,$DF,$DF
	db $DF,$E0,$E0,$E1,$E1,$E1,$E2,$E2,$E2,$E3,$E3,$E3,$E4,$E4,$E4,$E5
	db $E5,$E5,$E6,$E6,$E6,$E7,$E7,$E7,$E7,$E8,$E8,$E8,$E9,$E9,$E9,$EA
	db $EA,$EA,$EA,$EB,$EB,$EB,$EC,$EC,$EC,$EC,$ED,$ED,$ED,$ED,$EE,$EE
	db $EE,$EE,$EF,$EF,$EF,$EF,$F0,$F0,$F0,$F1,$F1,$F1,$F1,$F1,$F2,$F2
	db $F2,$F2,$F3,$F3,$F3,$F3,$F4,$F4,$F4,$F4,$F5,$F5,$F5,$F5,$F5,$F6
	db $F6,$F6,$F6,$F7,$F7,$F7,$F7,$F7,$F8,$F8,$F8,$F8,$F9,$F9,$F9,$F9
	db $F9,$FA,$FA,$FA,$FA,$FA,$FB,$FB,$FB,$FB,$FB,$FC,$FC,$FC,$FC,$FC
	db $FD,$FD,$FD,$FD,$FD,$FD,$FE,$FE,$FE,$FE,$FE,$FF,$FF,$FF,$FF,$FF
