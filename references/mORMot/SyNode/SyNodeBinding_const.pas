/// `util` module support bindings for SyNode
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SyNodeBinding_const;

interface
{$I Synopse.inc}
{$I SyNode.inc}
uses
  SysUtils,
  SynCommons,
  SyNode, SpiderMonkey;

implementation

function SyNodeBindingProc_consts(const aEngine: TSMEngine; const bindingNamespaceName: SynUnicode): jsval;
var
  obj, obj_fs: PJSRootedObject;
  jsv: jsval;
  cx: PJSContext;
const
  attrs = JSPROP_ENUMERATE or JSPROP_READONLY or JSPROP_PERMANENT;
begin
  cx := aEngine.cx;
  obj := cx.NewRootedObject(cx.NewObject(nil));
  obj_fs := cx.NewRootedObject(cx.NewObject(nil));
  try
   // file access modes
   jsv.asInteger := fmOpenRead; obj_fs.ptr.DefineProperty(cx, 'O_RDONLY', jsv, attrs);
   jsv.asInteger := fmOpenWrite; obj_fs.ptr.DefineProperty(cx, 'O_WRONLY', jsv, attrs);
   jsv.asInteger := fmOpenReadWrite; obj_fs.ptr.DefineProperty(cx, 'O_RDWR', jsv, attrs);

   jsv.asInteger := {$ifdef FPC}fmShareExclusive{$else}fmExclusive{$endif}; obj_fs.ptr.DefineProperty(cx, 'O_EXCL', jsv, attrs);

// TODO define all other consts
//   obj_fs.ptr.DefineProperty(cx, 'O_APPEND', , attrs)
//   obj_fs.ptr.DefineProperty(cx, 'O_CREAT', , attrs)
//   obj_fs.ptr.DefineProperty(cx, 'O_SYNC', , attrs)
//   obj_fs.ptr.DefineProperty(cx, 'O_TRUNC', , attrs)
//
//
//  NODE_DEFINE_CONSTANT(target, S_IFMT);
//  NODE_DEFINE_CONSTANT(target, S_IFREG);
//  NODE_DEFINE_CONSTANT(target, S_IFDIR);
//  NODE_DEFINE_CONSTANT(target, S_IFCHR);
//#ifdef S_IFBLK
//  NODE_DEFINE_CONSTANT(target, S_IFBLK);
//#endif
//
//#ifdef S_IFIFO
//  NODE_DEFINE_CONSTANT(target, S_IFIFO);
//#endif
//
//#ifdef S_IFLNK
//  NODE_DEFINE_CONSTANT(target, S_IFLNK);
//#endif
//
//#ifdef S_IFSOCK
//  NODE_DEFINE_CONSTANT(target, S_IFSOCK);
//#endif
//
//
//#ifdef O_NOCTTY
//  NODE_DEFINE_CONSTANT(target, O_NOCTTY);
//#endif
//
//#ifdef O_DIRECTORY
//  NODE_DEFINE_CONSTANT(target, O_DIRECTORY);
//#endif
//
//#ifdef O_EXCL
//  NODE_DEFINE_CONSTANT(target, O_EXCL);
//#endif
//
//#ifdef O_NOATIME
//  NODE_DEFINE_CONSTANT(target, O_NOATIME);
//#endif
//
//#ifdef O_NOFOLLOW
//  NODE_DEFINE_CONSTANT(target, O_NOFOLLOW);
//#endif
//
//#ifdef O_DSYNC
//  NODE_DEFINE_CONSTANT(target, O_DSYNC);
//#endif
//
//
//#ifdef O_SYMLINK
//  NODE_DEFINE_CONSTANT(target, O_SYMLINK);
//#endif
//
//#ifdef O_DIRECT
//  NODE_DEFINE_CONSTANT(target, O_DIRECT);
//#endif
//
//#ifdef O_NONBLOCK
//  NODE_DEFINE_CONSTANT(target, O_NONBLOCK);
//#endif
//
//#ifdef S_IRWXU
//  NODE_DEFINE_CONSTANT(target, S_IRWXU);
//#endif
//
//#ifdef S_IRUSR
//  NODE_DEFINE_CONSTANT(target, S_IRUSR);
//#endif
//
//#ifdef S_IWUSR
//  NODE_DEFINE_CONSTANT(target, S_IWUSR);
//#endif
//
//#ifdef S_IXUSR
//  NODE_DEFINE_CONSTANT(target, S_IXUSR);
//#endif
//
//#ifdef S_IRWXG
//  NODE_DEFINE_CONSTANT(target, S_IRWXG);
//#endif
//
//#ifdef S_IRGRP
//  NODE_DEFINE_CONSTANT(target, S_IRGRP);
//#endif
//
//#ifdef S_IWGRP
//  NODE_DEFINE_CONSTANT(target, S_IWGRP);
//#endif
//
//#ifdef S_IXGRP
//  NODE_DEFINE_CONSTANT(target, S_IXGRP);
//#endif
//
//#ifdef S_IRWXO
//  NODE_DEFINE_CONSTANT(target, S_IRWXO);
//#endif
//
//#ifdef S_IROTH
//  NODE_DEFINE_CONSTANT(target, S_IROTH);
//#endif
//
//#ifdef S_IWOTH
//  NODE_DEFINE_CONSTANT(target, S_IWOTH);
//#endif
//
//#ifdef S_IXOTH
//  NODE_DEFINE_CONSTANT(target, S_IXOTH);
//#endif
//
//#ifdef F_OK
//  NODE_DEFINE_CONSTANT(target, F_OK);
//#endif
//
//#ifdef R_OK
//  NODE_DEFINE_CONSTANT(target, R_OK);
//#endif
//
//#ifdef W_OK
//  NODE_DEFINE_CONSTANT(target, W_OK);
//#endif
//
//#ifdef X_OK
//  NODE_DEFINE_CONSTANT(target, X_OK);
//#endif
    obj.ptr.DefineProperty(cx, 'fs', obj_fs.ptr.ToJSValue, attrs);
    Result := obj.ptr.ToJSValue;
  finally
    cx.FreeRootedObject(obj_fs);
    cx.FreeRootedObject(obj);
  end;
end;

initialization
  TSMEngineManager.RegisterBinding('constants', SyNodeBindingProc_consts);

end.
