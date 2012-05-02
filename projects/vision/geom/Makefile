include ../../Makefile.include

demo:
	./horizon --points=pts2d.txt --image=../../../data/images/calibration/disk1.jpg
	./horizon2 ../../../data/videos/rcube.avi
	./rectify --points=pts2d.txt --image=../../../data/images/calibration/disk1.jpg
	./resection --points=pts3d.txt --image=../../../data/images/calibration/cal1.jpg
	./resection2 --points=pcube37.txt --image=../../../data/images/calibration/cube1.png
	./stereo --points1=pl.txt --image1=../../../data/images/calibration/cube3.png --points2=pr.txt --image2=../../../data/images/calibration/cube4.png

