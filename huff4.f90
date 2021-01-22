!Subrotina que cria ietogramas baseados no quarto quartil de Huff

!!! Implementação 26/10/2020
! Leonardo Romero Monteiro

PROGRAM huff

implicit none

! primeira coluna é o tempo e a segunda e a precipitação incremental adimensionalizados, dimensionalizado e o ietograma
real(8), allocatable :: hufa(:,:), iet(:,:)
real(8) :: td, dt, prec, intens, aux1, k, m, b, n, tr
integer :: i, j, it, itipo

write(*,*) "Criador de Hietograma de Projeto baseado no quarto quartil de Huff"
write(*,*) "Desenvolvedor: Leonardo Romero Monteiro"
write(*,*) 
write(*,*) "IDF do município de: 0 - generico; 1 - Tubarao; 2 - Florianppolis (Back, 2011)"
read(*,*) itipo


if (itipo == 1) then
k = 1123.22
m = 0.194
b = 14.685
n = 0.792

elseif (itipo == 2) then
k = 222.0
m = 0.1648
b = 0.0
n = 0.3835

else

write(*,*) "Digite os coeficientes: k, m, b, n"
read(*,*) k, m, b, n

endif

write(*,*) "informe o tempo de retorno (anos)"
read(*,*) tr

write(*,*) "Tempo de duracao da precipitacao em minutos"
read(*,*) td

write(*,*) "Intervalo de tempo em minutos"
read(*,*) dt

intens = k * tr**m / (td + b)**n

prec = intens * td/60.

it = nint(td/dt)
allocate(hufa(2,it+1),iet(2,it+1))

hufa = 0.
iet  = 0.

do i = 1, it+1
 hufa(1,i) = dt*(i-1.)/td
enddo

do i = 1, it+1
 if (hufa(1,i) <= 0.10) then
  hufa(2,i) = 0.4*hufa(1,i)

 elseif (hufa(1,i) <= 0.35) then
  hufa(2,i) = 0.3*hufa(1,i) + 0.01

 elseif (hufa(1,i) <= 0.45) then
  hufa(2,i) = 0.4*hufa(1,i) - 0.025

 elseif (hufa(1,i) <= 0.60) then
  hufa(2,i) = 0.6*hufa(1,i) - 0.115

 elseif (hufa(1,i) <= 0.65) then
  hufa(2,i) = 0.9*hufa(1,i) - 0.295

 elseif (hufa(1,i) <= 0.70) then
  hufa(2,i) = 1.2*hufa(1,i) - 0.49

 elseif (hufa(1,i) <= 0.75) then
  hufa(2,i) = 1.7*hufa(1,i) - 0.84

 elseif (hufa(1,i) <= 0.80) then
  hufa(2,i) = 2.2*hufa(1,i) - 1.215

 elseif (hufa(1,i) <= 0.85) then
  hufa(2,i) = 3.9*hufa(1,i) - 2.575

 elseif (hufa(1,i) <= 0.90) then
  hufa(2,i) = 3.6*hufa(1,i) - 2.320

 elseif (hufa(1,i) <= 0.95) then
  hufa(2,i) = 1.1*hufa(1,i) - 0.07

 else
  hufa(2,i) =  0.5*hufa(1,i) + 0.5

 endif

enddo

 iet(1,1) = hufa(1,i) * td
 iet(2,1) = 0.

do i = 2, it+1
 iet(1,i) = hufa(1,i) * td
 iet(2,i) = (hufa(2,i)-hufa(2,i-1)) * prec
enddo

! Plotagem do diagrama da simulação
open(11,file='huff4.dat',form='formatted')

do i = 1, it+1
 write(11,*) hufa(1,i), hufa(2,i)
enddo

open(10,file='hietograma_huff4.dat',form='formatted')

do i = 1, it+1
 write(10,*) iet(1,i), iet(2,i)
enddo

 close(10)
 close(11)

! para o linux plotar, desativado para viabilizar a adaptação de diferentes sistemas.
! call system('gnuplot plot.gnu')

write(*,*) "RESULTADO!"
write(*,*) "a intensidade (mm/h) e a precipitacao (mm) sao: ", intens, prec
write(*,*) 
write(*,*) "os arquivos huff4.dat e hietograma_huff4.dat foram criados!"
write(*,*) 
write(*,*) "caso tenha alguma sugestao para aperfeicoar o aplicativo mande um e-mail para leonardo.monteiro@udesc.br"
write(*,*) "insira um valor qualquer para finalizar"
read(*,*) itipo

END PROGRAM huff
