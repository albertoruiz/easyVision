-- save captured video
-- then you can convert the generated yuv to a nicer format:
-- $ ./record webcam1 [--save=file.yuv] [--wait] [--limit=numframes]
-- $ mencoder file.yuv -o file.avi -ovc lavc -fps 15

import EasyVision

main = run $ camera >>= saveWin yuv
