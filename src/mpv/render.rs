// Copyright (C) 2016  ParadoxSpiral
//
// This file is part of mpv-rs.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

use crate::{mpv::mpv_err, Error, Result};
use libmpv_sys::{
    self, mpv_handle, mpv_opengl_init_params, mpv_render_context, mpv_render_context_free,
    mpv_render_context_render, mpv_render_context_set_update_callback, mpv_render_frame_info,
    mpv_render_param,
};
use std::collections::HashMap;
use std::convert::From;
use std::ffi::{c_void, CStr, CString};
use std::os::raw::c_int;
use std::ptr;

type DeleterFn = unsafe fn(*mut c_void);

pub struct RenderContext {
    ctx: *mut mpv_render_context,
    update_callback_cleanup: Option<Box<dyn FnOnce()>>,
}

/// For initializing the mpv OpenGL state via RenderParam::OpenGLInitParams
pub struct OpenGLInitParams<GLContext> {
    /// This retrieves OpenGL function pointers, and will use them in subsequent
    /// operation.
    /// Usually, you can simply call the GL context APIs from this callback (e.g.
    /// glXGetProcAddressARB or wglGetProcAddress), but some APIs do not always
    /// return pointers for all standard functions (even if present); in this
    /// case you have to compensate by looking up these functions yourself when
    /// libmpv wants to resolve them through this callback.
    /// libmpv will not normally attempt to resolve GL functions on its own, nor
    /// does it link to GL libraries directly.
    pub get_proc_address: fn(ctx: &GLContext, name: &str) -> *mut c_void,

    /// Value passed as ctx parameter to get_proc_address().
    pub ctx: GLContext,
}

/// For RenderParam::FBO
pub struct FBO {
    pub fbo: i32,
    pub width: i32,
    pub height: i32,
}

#[repr(i32)]
#[derive(Clone)]
pub enum RenderFrameInfoFlag {
    Present = libmpv_sys::mpv_render_frame_info_flag_MPV_RENDER_FRAME_INFO_PRESENT,
    Redraw = libmpv_sys::mpv_render_frame_info_flag_MPV_RENDER_FRAME_INFO_REDRAW,
    Repeat = libmpv_sys::mpv_render_frame_info_flag_MPV_RENDER_FRAME_INFO_REPEAT,
    BlockVSync = libmpv_sys::mpv_render_frame_info_flag_MPV_RENDER_FRAME_INFO_BLOCK_VSYNC,
}

impl From<u64> for RenderFrameInfoFlag {
    // mpv_render_frame_info_flag is u32, but mpv_render_frame_info.flags is u64 o\
    fn from(val: u64) -> Self {
        let val = val as i32;
        match val {
            libmpv_sys::mpv_render_frame_info_flag_MPV_RENDER_FRAME_INFO_PRESENT => {
                RenderFrameInfoFlag::Present
            }
            libmpv_sys::mpv_render_frame_info_flag_MPV_RENDER_FRAME_INFO_REDRAW => {
                RenderFrameInfoFlag::Redraw
            }
            libmpv_sys::mpv_render_frame_info_flag_MPV_RENDER_FRAME_INFO_REPEAT => {
                RenderFrameInfoFlag::Repeat
            }
            libmpv_sys::mpv_render_frame_info_flag_MPV_RENDER_FRAME_INFO_BLOCK_VSYNC => {
                RenderFrameInfoFlag::BlockVSync
            }
            _ => panic!("Tried converting invalid value to RenderFrameInfoFlag"),
        }
    }
}

#[derive(Clone)]
pub struct RenderFrameInfo {
    pub flags: RenderFrameInfoFlag,
    pub target_time: i64,
}

pub enum RenderParamApiType {
    OpenGl,
    Software,
}

pub enum RenderParam<GLContext> {
    Invalid,
    ApiType(RenderParamApiType),
    InitParams(OpenGLInitParams<GLContext>),
    FBO(FBO),
    FlipY(bool),
    Depth(i32),
    ICCProfile(Vec<u8>),
    AmbientLight(i32),
    X11Display(*const c_void),
    WaylandDisplay(*const c_void),
    AdvancedControl(bool),
    NextFrameInfo(RenderFrameInfo),
    BlockForTargetTime(bool),
    SkipRendering(bool),
    /// (Width, Height)
    SoftwareSize((i32, i32)),
    SoftwareStride(usize),
    SoftwareFormat(*const u8),
    SoftwarePointer(*mut u8),
}

impl<C> From<&RenderParam<C>> for i32 {
    fn from(val: &RenderParam<C>) -> Self {
        match val {
            RenderParam::Invalid => libmpv_sys::mpv_render_param_type_MPV_RENDER_PARAM_INVALID,
            RenderParam::ApiType(_) => libmpv_sys::mpv_render_param_type_MPV_RENDER_PARAM_API_TYPE,
            RenderParam::InitParams(_) => libmpv_sys::mpv_render_param_type_MPV_RENDER_PARAM_OPENGL_INIT_PARAMS,
            RenderParam::FBO(_) => libmpv_sys::mpv_render_param_type_MPV_RENDER_PARAM_OPENGL_FBO,
            RenderParam::FlipY(_) => libmpv_sys::mpv_render_param_type_MPV_RENDER_PARAM_FLIP_Y,
            RenderParam::Depth(_) => libmpv_sys::mpv_render_param_type_MPV_RENDER_PARAM_DEPTH,
            RenderParam::ICCProfile(_) => libmpv_sys::mpv_render_param_type_MPV_RENDER_PARAM_ICC_PROFILE,
            RenderParam::AmbientLight(_) => libmpv_sys::mpv_render_param_type_MPV_RENDER_PARAM_AMBIENT_LIGHT,
            RenderParam::X11Display(_) => libmpv_sys::mpv_render_param_type_MPV_RENDER_PARAM_X11_DISPLAY,
            RenderParam::WaylandDisplay(_) => libmpv_sys::mpv_render_param_type_MPV_RENDER_PARAM_WL_DISPLAY,
            RenderParam::AdvancedControl(_) => libmpv_sys::mpv_render_param_type_MPV_RENDER_PARAM_ADVANCED_CONTROL,
            RenderParam::NextFrameInfo(_) => libmpv_sys::mpv_render_param_type_MPV_RENDER_PARAM_NEXT_FRAME_INFO,
            RenderParam::BlockForTargetTime(_) => libmpv_sys::mpv_render_param_type_MPV_RENDER_PARAM_BLOCK_FOR_TARGET_TIME,
            RenderParam::SkipRendering(_) => libmpv_sys::mpv_render_param_type_MPV_RENDER_PARAM_SKIP_RENDERING,
            RenderParam::SoftwareFormat(_) => libmpv_sys::mpv_render_param_type_MPV_RENDER_PARAM_SW_FORMAT,
            RenderParam::SoftwareSize(_) => libmpv_sys::mpv_render_param_type_MPV_RENDER_PARAM_SW_SIZE,
            RenderParam::SoftwareStride(_) => libmpv_sys::mpv_render_param_type_MPV_RENDER_PARAM_SW_STRIDE,
            RenderParam::SoftwarePointer(_) => libmpv_sys::mpv_render_param_type_MPV_RENDER_PARAM_SW_POINTER,
        }
    }
}

unsafe extern "C" fn gpa_wrapper<GLContext>(ctx: *mut c_void, name: *const i8) -> *mut c_void {
    if ctx.is_null() {
        panic!("ctx for get_proc_address wrapper is NULL");
    }

    let params: *mut OpenGLInitParams<GLContext> = ctx as _;
    let params = &*params;
    (params.get_proc_address)(
        &params.ctx,
        CStr::from_ptr(name)
            .to_str()
            .expect("Could not convert function name to str"),
    )
}

unsafe extern "C" fn ru_wrapper<F: Fn() + Send + 'static>(ctx: *mut c_void) {
    if ctx.is_null() {
        panic!("ctx for render_update wrapper is NULL");
    }

    (*(ctx as *mut F))();
}

impl<C> From<OpenGLInitParams<C>> for mpv_opengl_init_params {
    fn from(val: OpenGLInitParams<C>) -> Self {
        Self {
            get_proc_address: Some(gpa_wrapper::<OpenGLInitParams<C>>),
            get_proc_address_ctx: Box::into_raw(Box::new(val)) as *mut c_void,
            extra_exts: ptr::null(),
        }
    }
}

impl<C> From<RenderParam<C>> for mpv_render_param {
    fn from(val: RenderParam<C>) -> Self {
        let type_ = i32::from(&val);
        let data = match val {
            RenderParam::Invalid => ptr::null_mut(),
            RenderParam::ApiType(api_type) => match api_type {
                RenderParamApiType::OpenGl => {
                    libmpv_sys::MPV_RENDER_API_TYPE_OPENGL.as_ptr() as *mut c_void
                },
                RenderParamApiType::Software => {
                    libmpv_sys::MPV_RENDER_API_TYPE_SW.as_ptr() as *mut c_void
                }
            },
            RenderParam::InitParams(params) => {
                Box::into_raw(Box::new(mpv_opengl_init_params::from(params))) as *mut c_void
            }
            RenderParam::FBO(fbo) => Box::into_raw(Box::new(fbo)) as *mut c_void,
            RenderParam::FlipY(flip) => Box::into_raw(Box::new(flip as c_int)) as *mut c_void,
            RenderParam::Depth(depth) => Box::into_raw(Box::new(depth)) as *mut c_void,
            RenderParam::ICCProfile(bytes) => {
                Box::into_raw(bytes.into_boxed_slice()) as *mut c_void
            }
            RenderParam::AmbientLight(lux) => Box::into_raw(Box::new(lux)) as *mut c_void,
            RenderParam::X11Display(ptr) => ptr as *mut _,
            RenderParam::WaylandDisplay(ptr) => ptr as *mut _,
            RenderParam::AdvancedControl(adv_ctrl) => {
                Box::into_raw(Box::new(adv_ctrl as c_int)) as *mut c_void
            }
            RenderParam::NextFrameInfo(frame_info) => {
                Box::into_raw(Box::new(frame_info)) as *mut c_void
            }
            RenderParam::BlockForTargetTime(block) => {
                Box::into_raw(Box::new(block as c_int)) as *mut c_void
            }
            RenderParam::SkipRendering(skip_rendering) => {
                Box::into_raw(Box::new(skip_rendering as c_int)) as *mut c_void
            }
            RenderParam::SoftwareSize((width, height)) => {
                Box::into_raw(Box::new([width, height])) as *mut c_void
            }
            RenderParam::SoftwareStride(stride) => {
                Box::into_raw(Box::new(stride as libmpv_sys::size_t)) as *mut c_void
            }
            RenderParam::SoftwareFormat(format) => {
                format as *mut c_void
            }
            RenderParam::SoftwarePointer(pixels) => pixels as *mut c_void
        };
        Self { type_, data }
    }
}

unsafe fn free_void_data<T>(ptr: *mut c_void) {
    Box::<T>::from_raw(ptr as *mut T);
}

unsafe fn free_init_params<C>(ptr: *mut c_void) {
    let params = Box::from_raw(ptr as *mut mpv_opengl_init_params);
    Box::from_raw(params.get_proc_address_ctx as *mut OpenGLInitParams<C>);
}

impl RenderContext {
    pub fn new<C>(
        mpv: &mut mpv_handle,
        params: impl IntoIterator<Item = RenderParam<C>>,
    ) -> Result<Self> {
        let params: Vec<_> = params.into_iter().collect();
        let mut raw_params: Vec<mpv_render_param> = Vec::new();
        raw_params.reserve(params.len() + 1);
        let mut raw_ptrs: HashMap<*const c_void, DeleterFn> = HashMap::new();

        for p in params {
            // The render params are type-erased after they are passed to mpv. This is where we last
            // know their real types, so we keep a deleter here.
            let deleter: Option<DeleterFn> = match p {
                RenderParam::InitParams(_) => Some(free_init_params::<C>),
                RenderParam::FBO(_) => Some(free_void_data::<FBO>),
                RenderParam::FlipY(_) => Some(free_void_data::<i32>),
                RenderParam::Depth(_) => Some(free_void_data::<i32>),
                RenderParam::ICCProfile(_) => Some(free_void_data::<Box<[u8]>>),
                RenderParam::AmbientLight(_) => Some(free_void_data::<i32>),
                RenderParam::NextFrameInfo(_) => Some(free_void_data::<RenderFrameInfo>),
                _ => None,
            };
            let raw_param: mpv_render_param = p.into();
            if let Some(deleter) = deleter {
                raw_ptrs.insert(raw_param.data, deleter);
            }

            raw_params.push(raw_param);
        }
        // the raw array must end with type = 0
        raw_params.push(mpv_render_param {
            type_: 0,
            data: ptr::null_mut(),
        });

        unsafe {
            let raw_array = Box::into_raw(raw_params.into_boxed_slice()) as *mut mpv_render_param;
            let ctx = Box::into_raw(Box::new(std::ptr::null_mut() as _));
            let err = libmpv_sys::mpv_render_context_create(ctx, &mut *mpv, raw_array);
            Box::from_raw(raw_array);
            for (ptr, deleter) in raw_ptrs.iter() {
                (deleter)(*ptr as _);
            }

            mpv_err(
                Self {
                    ctx: *Box::from_raw(ctx),
                    update_callback_cleanup: None,
                },
                err,
            )
        }
    }

    pub fn set_parameter<C>(&self, param: RenderParam<C>) -> Result<()> {
        unsafe {
            mpv_err(
                (),
                libmpv_sys::mpv_render_context_set_parameter(
                    self.ctx,
                    mpv_render_param::from(param),
                ),
            )
        }
    }

    pub fn get_info<C>(&self, param: RenderParam<C>) -> Result<RenderParam<C>> {
        let is_next_frame_info = matches!(param, RenderParam::NextFrameInfo(_));
        let raw_param = mpv_render_param::from(param);
        let res = unsafe { libmpv_sys::mpv_render_context_get_info(self.ctx, raw_param) };
        if res == 0 {
            if !is_next_frame_info {
                panic!("I don't know how to handle this info type.");
            }
            let raw_frame_info = raw_param.data as *mut mpv_render_frame_info;
            unsafe {
                let raw_frame_info = *raw_frame_info;
                return Ok(RenderParam::NextFrameInfo(RenderFrameInfo {
                    flags: raw_frame_info.flags.into(),
                    target_time: raw_frame_info.target_time,
                }));
            }
        }
        Err(Error::Raw(res))
    }

    /// Render video.
    ///
    /// Typically renders the video to a target surface provided via `fbo`
    /// (the details depend on the backend in use). Options like "panscan" are
    /// applied to determine which part of the video should be visible and how the
    /// video should be scaled. You can change these options at runtime by using the
    /// mpv property API.
    ///
    /// The renderer will reconfigure itself every time the target surface
    /// configuration (such as size) is changed.
    ///
    /// This function implicitly pulls a video frame from the internal queue and
    /// renders it. If no new frame is available, the previous frame is redrawn.
    /// The update callback set with [set_update_callback](Self::set_update_callback)
    /// notifies you when a new frame was added. The details potentially depend on
    /// the backends and the provided parameters.
    ///
    /// Generally, libmpv will invoke your update callback some time before the video
    /// frame should be shown, and then lets this function block until the supposed
    /// display time. This will limit your rendering to video FPS. You can prevent
    /// this by setting the "video-timing-offset" global option to 0. (This applies
    /// only to "audio" video sync mode.)
    ///
    /// # Arguments
    ///
    /// * `fbo` - A framebuffer object to render to. In OpenGL, 0 is the current backbuffer
    /// * `width` - The width of the framebuffer in pixels. This is used for scaling the
    ///             video properly.
    /// * `height` - The height of the framebuffer in pixels. This is used for scaling the
    ///              video properly.
    /// * `flip` - Whether to draw the image upside down. This is needed for OpenGL because
    ///            it uses a coordinate system with positive Y up, but videos use positive
    ///            Y down.
    pub fn render<GLContext>(&self, fbo: i32, width: i32, height: i32, flip: bool) -> Result<()> {
        let mut raw_params: Vec<mpv_render_param> = Vec::with_capacity(3);
        let mut raw_ptrs: HashMap<*const c_void, DeleterFn> = HashMap::new();

        let raw_param: mpv_render_param =
            RenderParam::<GLContext>::FBO(FBO { fbo, width, height }).into();
        raw_ptrs.insert(raw_param.data, free_void_data::<FBO>);
        raw_params.push(raw_param);
        let raw_param: mpv_render_param = RenderParam::<GLContext>::FlipY(flip).into();
        raw_ptrs.insert(raw_param.data, free_void_data::<i32>);
        raw_params.push(raw_param);
        // the raw array must end with type = 0
        raw_params.push(mpv_render_param {
            type_: 0,
            data: ptr::null_mut(),
        });

        let raw_array = Box::into_raw(raw_params.into_boxed_slice()) as *mut mpv_render_param;

        let ret = unsafe { mpv_err((), mpv_render_context_render(self.ctx, raw_array)) };
        unsafe {
            Box::from_raw(raw_array);
        }

        unsafe {
            for (ptr, deleter) in raw_ptrs.iter() {
                (deleter)(*ptr as _);
            }
        }

        ret
    }

    pub fn render_sw(&self, size: (i32, i32), format: &str, stride: usize, array_to_render: &mut [u8]) -> Result<()> {
        let mut raw_params: Vec<mpv_render_param> = Vec::with_capacity(3);
        let mut raw_ptrs: HashMap<*const c_void, DeleterFn> = HashMap::new();

        let raw_param: mpv_render_param =
            RenderParam::<()>::SoftwareStride(stride).into();
        raw_ptrs.insert(raw_param.data, free_void_data::<usize>);
        raw_params.push(raw_param);

        let raw_param: mpv_render_param = RenderParam::<()>::SoftwareFormat(format.as_ptr()).into();
        // No need to free as we only have a reference to it
        raw_params.push(raw_param);

        let raw_param: mpv_render_param = RenderParam::<()>::SoftwareSize(size).into();
        raw_ptrs.insert(raw_param.data, free_void_data::<[i32; 2]>);
        raw_params.push(raw_param);

        let raw_param: mpv_render_param = RenderParam::<()>::SoftwarePointer(array_to_render.as_mut_ptr()).into();
        // No need to free as we only have a reference to it
        raw_params.push(raw_param);

        // the raw array must end with type = 0
        raw_params.push(mpv_render_param {
            type_: 0,
            data: ptr::null_mut(),
        });

        let raw_array = Box::into_raw(raw_params.into_boxed_slice()) as *mut mpv_render_param;

        let ret = unsafe { mpv_err((), mpv_render_context_render(self.ctx, raw_array)) };
        unsafe {
            Box::from_raw(raw_array); // Free memory
        }

        unsafe {
            for (ptr, deleter) in raw_ptrs.iter() {
                (deleter)(*ptr as _);
            }
        }

        ret
    }

    /// Set the callback that notifies you when a new video frame is available, or if the video display
    /// configuration somehow changed and requires a redraw. Similar to [EventContext::set_wakeup_callback](crate::events::EventContext::set_wakeup_callback), you
    /// must not call any mpv API from the callback, and all the other listed restrictions apply (such
    /// as not exiting the callback by throwing exceptions).
    ///
    /// This can be called from any thread, except from an update callback. In case of the OpenGL backend,
    /// no OpenGL state or API is accessed.
    ///
    /// Calling this will raise an update callback immediately.
    pub fn set_update_callback<F: Fn() + Send + 'static>(&mut self, callback: F) {
        if let Some(update_callback_cleanup) = self.update_callback_cleanup.take() {
            update_callback_cleanup();
        }
        let raw_callback = Box::into_raw(Box::new(callback));
        self.update_callback_cleanup = Some(Box::new(move || unsafe {
            Box::from_raw(raw_callback);
        }) as Box<dyn FnOnce()>);
        unsafe {
            mpv_render_context_set_update_callback(
                self.ctx,
                Some(ru_wrapper::<F>),
                raw_callback as *mut c_void,
            );
        }
    }
}

impl Drop for RenderContext {
    fn drop(&mut self) {
        if let Some(update_callback_cleanup) = self.update_callback_cleanup.take() {
            update_callback_cleanup();
        }
        unsafe {
            mpv_render_context_free(self.ctx);
        }
    }
}
