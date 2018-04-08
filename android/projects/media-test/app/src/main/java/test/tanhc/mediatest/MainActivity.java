package test.tanhc.mediatest;

import android.hardware.Camera;
import android.media.MediaCodec;
import android.media.MediaCodecInfo;
import android.media.MediaCodecList;
import android.media.MediaFormat;
import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.util.Log;
import android.view.SurfaceHolder;
import android.view.SurfaceView;

public class MainActivity extends AppCompatActivity {
    private static final String TAG = "MainActivity";

    private SurfaceView mSurfaceView;
    private Camera mCamera;
    private MediaCodec mEncoder;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        mSurfaceView = findViewById(R.id.surfaceView);
        assert mSurfaceView != null;

        mSurfaceView.getHolder().addCallback(new SurfaceHolder.Callback() {
            @Override
            public void surfaceCreated(SurfaceHolder holder) {

            }

            @Override
            public void surfaceChanged(SurfaceHolder holder, int format, int width, int height) {
                closeCamera();

                mCamera = Camera.open();
                mCamera.setDisplayOrientation(90);

                Camera.Parameters parameters = mCamera.getParameters();
                parameters.setFlashMode(Camera.Parameters.FLASH_MODE_OFF);
                parameters.setFocusMode(Camera.Parameters.FOCUS_MODE_CONTINUOUS_VIDEO);
                // parameters.setPreviewSize(640, 480);
                // Log.i(TAG, ""+parameters.getPreviewFormat());
                mCamera.setParameters(parameters);
                try {
                    mCamera.setPreviewDisplay(holder);
                    mCamera.startPreview();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            @Override
            public void surfaceDestroyed(SurfaceHolder holder) {
                closeCamera();
            }

            private void closeCamera() {
                if (mCamera != null) {
                    mCamera.stopPreview();
                    mCamera.release();
                    mCamera = null;
                }
            }
        });

        initEncoder();
    }

    private MediaCodecInfo selectCodec(String mimeType) {
        int count = MediaCodecList.getCodecCount();

        for (int i = 0; i < count; i++) {
            MediaCodecInfo codecInfo = MediaCodecList.getCodecInfoAt(i);
            if (!codecInfo.isEncoder()) {
                continue;
            }

            String[] types = codecInfo.getSupportedTypes();
            for (String type : types) {
                if (type.equalsIgnoreCase(mimeType)) {
                    return codecInfo;
                }
            }
        }

        return null;
    }

    private void initEncoder() {
        MediaCodecInfo codecInfo = selectCodec("video/avc");
        MediaCodecInfo.CodecCapabilities capabilities = codecInfo.getCapabilitiesForType("video/avc");

        try {
            mEncoder = MediaCodec.createEncoderByType("video/avc");
            MediaFormat format = MediaFormat.createVideoFormat("video/avc", 640, 480);
            format.setInteger(MediaFormat.KEY_BIT_RATE, 500000);
            format.setInteger(MediaFormat.KEY_FRAME_RATE, 15);
            format.setInteger(MediaFormat.KEY_COLOR_FORMAT,
                    MediaCodecInfo.CodecCapabilities.COLOR_FormatYUV420Flexible);
            format.setInteger(MediaFormat.KEY_I_FRAME_INTERVAL, 5);
            mEncoder.configure(format, null, null, MediaCodec.CONFIGURE_FLAG_ENCODE);
            mEncoder.start();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
