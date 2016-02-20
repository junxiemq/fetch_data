FC=ifort
#FC=gfortran
saclib=/home/junxie/opt/sac/lib/sacio.a
#saclib=/usr/local/sac/lib/sacio.a
#saclib=/Users/junxie/opt/sac/lib/sacio.a
fflag=-ffixed-line-length-none
subjects=fetch_data.o wrsac.o readsac.o abstime.o decimate.o rm_resp.o
#all:extract
fetch_data:$(subjects)
	$(FC) $(subjects) -o $@ $(saclib) $(fflag)
%.o:%.f90
	$(FC) $^ -c $(fflag)
clean:
	rm $(subjects)
