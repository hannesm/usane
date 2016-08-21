#include <stdint.h>

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>

#define Uint32_val(v) (*((uint32_t *) Data_custom_val(v)))

CAMLprim value
caml_uint32_add_overflow (value a, value b) {
  uint32_t ua, ub, uc;
  CAMLparam0();
  ua = Uint32_val(a);
  ub = Uint32_val(b);
  if (__builtin_uadd_overflow(ua, ub, &uc))
    caml_invalid_argument("overflow");
  else
    CAMLreturn(caml_copy_int32(uc));
}

CAMLprim value
caml_uint32_sub_underflow (value a, value b) {
  uint32_t ua, ub, uc;
  CAMLparam0();
  ua = Uint32_val(a);
  ub = Uint32_val(b);
  if (__builtin_usub_overflow(ua, ub, &uc))
    caml_invalid_argument("underflow");
  else
    CAMLreturn(caml_copy_int32(uc));
}
