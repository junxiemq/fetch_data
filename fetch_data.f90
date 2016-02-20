! fetch seed data
! and extract
program main
parameter (nn=21048576)
integer i,j,nsta,error,id
integer byear,eyear,bday,eday,iyear,iday,bmonth,emonth
integer month_bday,month_eday
integer imonth,nerr
integer ibmonth,iemonth
integer ibday,ieday,icom,jmon
integer do_decimate,do_rm_response,do_extract
integer n_sta,jday,jday1
integer npts,nzero,imark,n1,nlen
integer nzhour,nzmin,nzsec,nzmsec
real f1,f2,dt0
real sigall(nn),sig(nn),sigo(nn)
real dt,beg,stla,stlo
integer,dimension(12):: monday=(/31,28,31,30,31,30,31,31,30,31,30,31/)
character(10),dimension(1000) :: stalist,netlist
character(80)  :: args,user,label
character(80)  :: sta_list
character(3)   :: component
character(3)   :: com(3),co
character(80)  :: chars,seed,mdata
character(100) :: mail,saclist
character(180) :: command
character(180) :: sac_tmp,sac
character(180) :: para,file,output_seed
character(80)  :: dirout,bash,resp
character(10)  :: year_day
character(2)   :: kh,kho,khole
logical ext
component="BHZ"
if(iargc().lt.1)then
      write(*,*)'Usage: fetch_data param.dat'
      write(*,*)'param.dat:'
      write(*,*)'station list: station, network, component'
      write(*,*)'year_b day_b'
      write(*,*)'year_e day_e'
      write(*,*)'component icom'
      write(*,*)'f1   f2  identity (do remove response (1) or not (0)) identity (do decimate or not) do_extract'
      write(*,*)'dir_of_output'
      call exit(-1)
endif
call getarg(1,para)
! read in control parameter
open(9,file=para)
read(9,'(a80)')sta_list
read(9,*)byear,bday
read(9,*)eyear,eday
read(9,*)co,icom
read(9,*)f1,f2,do_rm_response,do_decimate,do_extract
read(9,'(a80)')dirout
close(9)
if(icom.eq.1)then
   com(1)=co
else
   com(1)=trim(co)//'Z'
   com(2)=trim(co)//'N'
   com(3)=trim(co)//'E'
endif

if(byear.gt.eyear)then
      call exit(-1)
endif
if(byear.eq.eyear.and.bday.gt.eday)then
      call exit(-1)
endif
user="tmp"
label="tmp"
inquire(file=sta_list,exist=ext)
if(.not.ext)then
      call exit(-1)
endif
! read station list
i=1
open(13,file=sta_list)
8 read(13,'(a7,a2)',end=9,err=9)stalist(i),netlist(i)
  i=i+1
  goto 8
9 continue
n_sta=i-1 !number of station
write(*,*)'n_sta=',n_sta
!write(*,*)'number of station is ',n_sta
close(13)
! find day of month at begin time
if(mod(byear,4)==0.and.mod(byear,100).ne.0.or.mod(byear,400).eq.0)monday(2)=29  
month_bday=0
i=1
10 month_bday=month_bday+monday(i)
if(bday.le.month_bday)then
   bmonth=i
else
   i=i+1
   goto 10
endif
month_bday=bday-(month_bday-monday(i))
!write(*,*)byear,bmonth,month_bday

! find day of month at end time
if(mod(eyear,4)==0.and.mod(eyear,100).ne.0.or.mod(eyear,400).eq.0)monday(2)=29  
month_eday=0
i=1
11 month_eday=month_eday+monday(i)
if(eday.le.month_eday)then
   emonth=i
else
   i=i+1
   goto 11
endif
month_eday=eday-(month_eday-monday(i))
!write(*,*)eyear,emonth,month_eday
call system("mkdir seed 2>/dev/null")
call system("mkdir resp 2>/dev/null")
call system("mkdir mdat 2>/dev/null")
do iyear=byear,eyear
     monday(2)=28
     if(mod(iyear,4)==0.and.mod(iyear,100).ne.0.or.mod(iyear,400).eq.0)monday(2)=29  
     ibmonth=1
     iemonth=12
     if(iyear.eq.byear)ibmonth=bmonth
     if(iyear.eq.eyear)iemonth=emonth
     jday=0
     do imonth=ibmonth,iemonth
            jday1=0
            if(imonth.eq.1)then
                 jday1=0       
            else
            do jmon=1,imonth-1
                  jday1=jday1+monday(jmon)
            enddo
            endif
            ibday=1
            ieday=monday(imonth)
            if(iyear.eq.byear.and.imonth.eq.bmonth)ibday=month_bday
            if(iyear.eq.eyear.and.imonth.eq.emonth)ieday=month_eday
            do iday=ibday,ieday
                 jday=jday1+iday
                 write(output_seed,'("seed/",i4.4,"_",i3.3,".seed")')iyear,jday
                 inquire(file=output_seed,exist=ext)
                 if (.not.ext)then
                         write(command,'("touch",1x,1a)')trim(output_seed)
                         call system(command)
                 write(*,'("jday=",i3.3)')jday
                 write(*,'("year=",i4.4,1x,"month=",i2.2,1x,"day=",i2.2)')iyear,imonth,iday
                 write(year_day,'(i4.4,"_",i3.3)')iyear,jday
                 write(seed,'(i4.4,"_",i3.3,".seed")')iyear,jday
                 write(mdata,'(i4.4,"_",i3.3,".mdata")')iyear,jday
                 write(mail,'(i4.4,"_",i3.3,".info")')iyear,jday
                 open(12,file=mail)
                 write(12,'(".NAME",1x,1a)')user
                 write(12,'(".INST Macquarie University")')
                 write(12,'(".MAIL Macquarie University, Sydney")')
                 write(12,'(".EMAIL junxie01@gmail.com ")')
                 write(12,'(".PHONE 8613721050718")')
                 write(12,'(".FAX 5555513")')
                 write(12,'(".MEDIA: Electronic (FTP) ")')
                 write(12,'(".ALTERNATE MEDIA :  Electronic (FTP)")')
                 write(12,'(".ALTERNATE MEDIA :  Electronic (FTP)")')
                 write(12,'(".LABEL",1x,1a)')label
                 write(12,'(".QUALITY B")')
                 write(12,'(".END")')
                 write(12,'("")')
                 do ista=1,n_sta
                        do ic=1,icom
                        write(12,'(1a7,1x,1a2,1x,i4.4,1x,i2.2,1x,i2.2,1x,"00 00 00.000",&
                        1x,i4.4,1x,i2.2,1x,i2.2,1x,"23 59 59.999 1",1x,1a3)')stalist(ista),netlist(ista),&
                        iyear,imonth,iday,iyear,imonth,iday,com(ic)
                        enddo
                 enddo
                 close(12)
                 write(command,'("mkdir -p resp/resp_",i4.4,"_",i3.3,1x,"2>/dev/null")')iyear,jday
                 call system(command)
                 write(command,'("FetchData -b",1x,1a14,1x,"-o",1x,1a14,1x,"-m",1x,1a,1x,"-rd resp/resp_",i4.4,"_",i3.3)')trim(mail),trim(seed),trim(mdata),iyear,jday
                 !write(command,'("FetchData -b",1x,1a14,1x,"-rd .")')trim(mail)
                 !write(*,*)command
                 call system(command)
                 write(command,'("rm",1x,1a)') mail 
                 call system(command)
                 write(command,'("mv",1x,1a,1x,"seed")')seed
                 call system(command)
                 !call system("mv *.seed seed 2>/dev/null")
!                 write(command,'("mv RESP* resp/resp_",i4.4,"_",i3.3,1x,"2>/dev/null")')iyear,jday
!                 call system(command)
                 call system("mv *.mdata mdat 2>/dev/null")
                 if(do_extract.eq.1)then
                 write(command,'("mseed2sac ",1a,1x,"-m",1x,1a)')trim(seed),trim(mdata) 
                 call system(command)
                 write(command,'("mkdir -p",1x,1a,1x,"2>/dev/null")')trim(dirout)//'/'//trim(year_day)
                 call system(command)
                 do ista=1,n_sta
                         write(sac_tmp,'(i0,".",i3.3,"*",1a,"*",1a,"*SAC")')iyear,jday,trim(stalist(ista)),com(1)
                         write(sac_tmp,'(1a,".",1a,".*.",1a,".*.",i4.4,".",i3.3,"*.SAC")')trim(netlist(ista)),trim(stalist(ista)),com(1),iyear,jday
                         write(*,'("sac_tmp=",1a)')sac_tmp
!                         sac=trim(dirout)//'/'//trim(year_day)//'/'//trim(year_day)'_'//trim(stalist(ista))//'_'//trim(com(1))//'.SAC'
                         write(sac,'(1a,"/",1a,"/",1a,"_",1a,"_",1a,".SAC")') &
                         trim(dirout),trim(year_day),trim(year_day),trim(stalist(ista)),trim(com(1))
                         sigall=0
                         inquire(file=sac,exist=ext)
                         write(saclist,'(1a,"_",1a,"_",1a,".list")')trim(year_day),trim(stalist(ista)),com(1)
                         if (.not.ext)then
                               bash=trim(year_day)//"_"//trim(stalist(ista))//'.bash'
                               open(20,file=bash) 
                               write(20,'("#!/bin/bash")')
                               write(20,'("rm",1x,1a,1x,"2>/dev/null")')trim(saclist)
                               write(20,'("n=`ls",1x,1a,1x,"2>/dev/null | wc -l`")')trim(sac_tmp)
                               write(20,'("if [ $n -ge 1 ];then")')
                               write(20,'("     ls",1x,1a,1x,">",1a)')trim(sac_tmp),trim(saclist)
                               write(20,'("fi")')
                               close(20)
                               write(command,'("bash",1x,1a)')trim(bash)
                               call system(command) 
                               inquire(file=trim(saclist),exist=ext)  
                               imark=0
                               if(ext)then ! if the sac file list exists
                                       open(10,file=trim(saclist))
                                   13  file=""
                                       read(10,'(1a180)',err=20,end=20)file
                                       sig=0;stla=0;stlo=0;nzhour=0;nzsec=0;nzmsec=0
                                       nlen=0;dt=0;beg=0
                                       call readsac(trim(file),sig,nlen,dt,beg,stla,stlo,nzhour,nzmin,nzsec,nzmsec,khole,nerr) 
                                       if(imark==0)then
                                              kho=khole 
                                              write(kho,'(1a1,"0")')khole
                                              imark=imark+1
                                              dt0=dt
                                       endif
                                       write(kh,'(1a1,"0")')khole
                                       if(nerr.ne.0)goto 13 ! read file correctly
                                       if(kh.ne.kho)goto 13
                                       if(dt.ne.dt0)goto 13
                                       write(*,*)"read file ",trim(file)," khole=",trim(kh)
                                       call abstime(n1,nzhour,nzmin,nzsec,nzmsec,nlen,dt,sig,sigo)
                                       !write(*,'("n1=",1i)')n1
                                       if(n1.ge.1)then
                                              do i=1,nlen 
                                                     sigall(i+n1-1)=sigo(i)
                                              enddo
                                       else
                                              do i=1,nlen
                                                     if(i+n1-1.gt.0)then
                                                             sigall(i+n1-1)=sigo(i)
                                                     endif
                                              enddo
                                       endif
                                       goto 13 
                               20      continue
                                       close(10)
                                       npts=int(24*3600/dt)+1
                                       nzero=0 
                                       do i=1,npts
                                              if(sigall(i).eq.0)nzero=nzero+1 
                                       enddo
                                       if(nzero.lt.npts/2)then ! ignore the data with number of zeros bigger than half of the npts
                                              write(*,*)'write to file ',trim(sac)," khole=",kh
                                              if(kho.eq."10".or.kho.eq."00")then
                                                       call wrsac(trim(sac),dt0,sigall,npts,stla,stlo,&
                                                       iyear,jday,0,0,0,0,trim(stalist(ista)),&
                                                       trim(netlist(ista)),trim(com(1)),kho,nerr)
                                              else 
                                                       call wrsac(trim(sac),dt0,sigall,npts,stla,stlo,&
                                                       iyear,jday,0,0,0,0,trim(stalist(ista)),&
                                                       trim(netlist(ista)),trim(com(1)),"",nerr)
                                              endif
                                              if(do_decimate.eq.1)call decimate(trim(sac),dt0) ! decimate the data to 10Hz
                                              if(do_rm_response.eq.1)then
                                                      write(resp,'("RESP.",1a,".",1a,".",1a,".",1a)')trim(netlist(ista)),&
                                                      trim(stalist(ista)),trim(kho),trim(com(1))
                                                      if(trim(kho).ne."10".and.trim(kho).ne."00")&
                                                      write(resp,'("RESP.",1a,".",1a,"..",1a)')&
                                                      trim(netlist(ista)),trim(stalist(ista)),trim(com(1))
                                                      call rm_resp(bash,sac,trim(resp),f1,f2,error)
                                              endif
                                              write(*,'("station:",1x,1a)')trim(stalist(ista))
                                              write(*,'("network:",1x,1a)')trim(netlist(ista))
                                              write(*,'("component:",1x,1a)')trim(com(1))
                                              write(*,'("resp:",1x,1a)')trim(resp)
                                              if(error.eq.-1)then
                                                    write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(sac)
                                                    call system(command)
                                              endif
                                      endif
                                      write(command,'("rm",1x,1a,1x,"2>/dev/null")') trim(saclist)
                                      call system(command)
                               endif
                               write(command,'("rm",1x,1a,1x,"2>/dev/null")') trim(bash)
                               call system(command)
                               write(command,'("rm",1x,1a,1x,"2>/dev/null")')trim(sac_tmp)
                               call system(command)
                         endif
                 enddo ! loop over station
         endif
                 endif
            enddo ! loop over day
     enddo ! loop over month
enddo ! loop over year
!write(*,'("monday=",12i10)')(monday(i),i=1,12)
end program
