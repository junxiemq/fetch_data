subroutine abstime(n1,nzhour,nzmin,nzsec,nzmsec,npts,dt,sig,sigo)
real deltat
real sig(npts),sigo(npts)
integer n1,nzhour,nzmin,nzsec,nzmsec,npts
integer n1temp
!if(nzhour.ge.0)then
      deltat=mod(nzmsec,int(dt*1000))/1000.0
!else
!      deltat=mod(1000-nzmsec,int(dt*1000))/1000.0
!endif
nzmsec=int(nzmsec/dt/1000)*dt*1000
n1temp=t1/dt
do i=1,npts
       sigo(i)=sig(i)+deltat*(sig(i+1)-sig(i))/dt
enddo
n1=int(((nzhour)*60.0*60.0+nzmin*60.0+nzsec*1.0+nzmsec*1.0/1000.0)/dt)+1
!write(*,'("kztime=",4i,1x,"nnh=",1i)')nzhour,nzmin,nzsec,nzmsec,nnh
end subroutine
