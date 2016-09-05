#include <stdint.h>
#include <stdbool.h>

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>

#ifdef __clang__
#if !  __has_builtin(__builtin_uadd_overflow)
#define NEED 1
#endif
#else
#if (__GNUC__ * 100 + __GNUC_MINOR__) < 501
#define NEED 1
#endif
#endif

#ifdef NEED
#define __unsigned_add_overflow(a, b, d) ({	\
	typeof(a) __a = (a);			\
	typeof(b) __b = (b);			\
	typeof(d) __d = (d);			\
	(void) (&__a == &__b);			\
	(void) (&__a == __d);			\
	*__d = __a + __b;			\
	*__d < __a;				\
})

#define __type_half_max(type) ((type)1 << (8*sizeof(type) - 1))
#define type_max(T) ((T)((__type_half_max(T) - 1) + __type_half_max(T)))
#define __unsigned_mul_overflow(a, b, d) ({		\
	typeof(a) __a = (a);				\
	typeof(b) __b = (b);				\
	typeof(d) __d = (d);				\
	(void) (&__a == &__b);				\
	(void) (&__a == __d);				\
	*__d = __a * __b;				\
	__b > 0 && __a > type_max(typeof(__a)) / __b;	\
})

#define __unsigned_sub_overflow(a, b, d) ({	\
	typeof(a) __a = (a);			\
	typeof(b) __b = (b);			\
	typeof(d) __d = (d);			\
	(void) (&__a == &__b);			\
	(void) (&__a == __d);			\
	*__d = __a - __b;			\
	__a < __b;				\
})

#else
#define __unsigned_add_overflow __builtin_add_overflow
#define __unsigned_mul_overflow __builtin_mul_overflow
#define __unsigned_sub_overflow __builtin_sub_overflow
#endif

#define Uint32_val(v) (*((uint32_t *) Data_custom_val(v)))

CAMLprim value
caml_uint32_add_overflow (value a, value b) {
  uint32_t ua, ub, uc;
  bool carry;
  CAMLparam2(a, b);
  ua = Uint32_val(a);
  ub = Uint32_val(b);
  carry = __unsigned_add_overflow(ua, ub, &uc);
  CAMLlocal1(res);
  res = caml_alloc_tuple(2);
  Store_field(res, 0, caml_copy_int32(uc));
  Store_field(res, 1, Val_bool(carry));
  CAMLreturn(res);
}

CAMLprim value
caml_uint32_mul_overflow (value a, value b) {
  uint32_t ua, ub, uc;
  bool carry;
  CAMLparam2(a, b);
  ua = Uint32_val(a);
  ub = Uint32_val(b);
  carry = __unsigned_mul_overflow(ua, ub, &uc);
  CAMLlocal1(res);
  res = caml_alloc_tuple(2);
  Store_field(res, 0, caml_copy_int32(uc));
  Store_field(res, 1, Val_bool(carry));
  CAMLreturn(res);
}

CAMLprim value
caml_uint32_sub_underflow (value a, value b) {
  uint32_t ua, ub, uc;
  bool carry;
  CAMLparam2(a, b);
  ua = Uint32_val(a);
  ub = Uint32_val(b);
  carry = __unsigned_sub_overflow(ua, ub, &uc);
  CAMLlocal1(res);
  res = caml_alloc_tuple(2);
  Store_field(res, 0, caml_copy_int32(uc));
  Store_field(res, 1, Val_bool(carry));
  CAMLreturn(res);
}


#define Uint64_val(v) (*((uint64_t *) Data_custom_val(v)))

CAMLprim value
caml_uint64_add_overflow (value a, value b) {
  uint64_t ua, ub, uc;
  bool carry;
  CAMLparam2(a, b);
  ua = Uint64_val(a);
  ub = Uint64_val(b);
  carry = __unsigned_add_overflow(ua, ub, &uc);
  CAMLlocal1(res);
  res = caml_alloc_tuple(2);
  Store_field(res, 0, caml_copy_int64(uc));
  Store_field(res, 1, Val_bool(carry));
  CAMLreturn(res);
}

CAMLprim value
caml_uint64_mul_overflow (value a, value b) {
  uint64_t ua, ub, uc;
  bool carry;
  CAMLparam2(a, b);
  ua = Uint64_val(a);
  ub = Uint64_val(b);
  carry = __unsigned_mul_overflow(ua, ub, &uc);
  CAMLlocal1(res);
  res = caml_alloc_tuple(2);
  Store_field(res, 0, caml_copy_int64(uc));
  Store_field(res, 1, Val_bool(carry));
  CAMLreturn(res);
}

CAMLprim value
caml_uint64_sub_underflow (value a, value b) {
  uint64_t ua, ub, uc;
  bool carry;
  CAMLparam2(a, b);
  ua = Uint64_val(a);
  ub = Uint64_val(b);
  carry = __unsigned_sub_overflow(ua, ub, &uc);
  CAMLlocal1(res);
  res = caml_alloc_tuple(2);
  Store_field(res, 0, caml_copy_int64(uc));
  Store_field(res, 1, Val_bool(carry));
  CAMLreturn(res);
}
