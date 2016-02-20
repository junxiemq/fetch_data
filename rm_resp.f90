subroutine rm_resp(bash,sac,resp,f2,f3,err)
! remove response
character(*) bash,sac,resp
character(180) command,output,num_out
integer num,err
real f1,f2,f3,f4
f1=0.98*f2
f4=1.02*f3
err=0
write(output,'(1a,"_output")')trim(bash)
write(num_out,'(1a,"_num")')trim(bash)
open(20,file=bash)
write(20,'("sac<<eof >",1a)')output
write(20,'("r",1x,1a)')trim(sac)
write(20,'("rtr")')
write(20,'("rmean")')
!write(20,'("trans from evalresp fname",1x,1a,1x,"to vel freq 0.09 0.1 105 110")') trim(resp)
write(20,'("trans from evalresp to vel freq",4f9.4)')f1,f2,f3,f4
write(20,'("w over")')
write(20,'("quit")')
write(20,'("eof")')
close(20)
write(command,'("bash",1x,1a)')trim(bash)
call system(command)          ! remove response
write(command,'("grep -i ERROR",1x,1a,1x,"| wc -l >",1a)')trim(output),trim(num_out)
call system(command)
open(10,file=num_out)
read(10,*)num
close(10)
if(num.ge.1)then
       write(*,'("Something wrong with response file of",1x,1a)')trim(sac)
       err=-1 
endif
write(command,'("rm",1x,1a)')trim(output)
call system(command)
write(command,'("rm",1x,1a)')trim(num_out)
call system(command)
return
end subroutine
