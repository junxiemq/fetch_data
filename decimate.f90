subroutine decimate(name,dt)
! resample the file to 5/s
character(*)name
character(180)bash
character(180)command
real dt
bash=trim(name)//'_decimate.bash'
if(dt.ne.0.01)then
      open(10,file=trim(bash))
      write(10,'("sac<< eof >/dev/null")')
      write(10,'("r ",1a)')trim(name)
      if(abs(dt-0.005).lt.1e-4)then
             write(10,'("decimate 2")')
             write(10,'("decimate 2")')
            ! write(10,'("decimate 5")')
      !else if (abs(dt-0.02).lt.1e-4)then
      !       write(10,'("decimate 5")')
      !       write(10,'("decimate 2")')
            ! write(10,'("decimate 5")')
      !else if (abs(dt-0.01).lt.1e-4)then
      !       write(10,'("decimate 5")')
      !       write(10,'("decimate 2")')
            ! write(10,'("decimate 5")')
      !       write(10,'("decimate 2")')
!               else if (abs(dt-0.1).lt.1e-4)then
!                         write(10,'("decimate 2")')
      endif
      write(10,'("w over")')
      write(10,'("q")')
      write(10,'("eof")')
      close(10)
      write(command,'("bash ",1a)')trim(bash)
      call system(command)
      write(command,'("rm",1x,1a)')trim(bash)
      call system(command)
endif
end subroutine
