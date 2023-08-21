; ModuleID = 'std::tvalue'
source_filename = "std::tvalue"

%enum.TValue = type { i8, [8 x i8] }
%tvalue_bool = type { [1 x i64], i8 }
%tvalue_string = type { [1 x i8], ptr }

define i8 @getTValueTag(ptr %tagged_value) {
entry:
  %tag = alloca i8, align 1
  %tag_ptr = getelementptr inbounds { i8, [8 x i8] }, ptr %tagged_value, i32 0, i32 0
  %tag_val = load i8, ptr %tag_ptr, align 1
  store i8 %tag_val, ptr %tag, align 1
  %ret_val = load i8, ptr %tag, align 1
  ret i8 %ret_val
}

define i1 @getTValueValue_bool(ptr %tagged_value) {
entry:
  %tag = call i8 @getTValueTag(ptr %tagged_value)
  %is_nil = icmp eq i8 %tag, 0
  br i1 %is_nil, label %is_nil1, label %is_not_nil

is_nil1:                                          ; preds = %entry
  ret i1 false

is_not_nil:                                       ; preds = %entry
  %is_bool = icmp eq i8 %tag, 1
  br i1 %is_bool, label %is_bool2, label %is_not_bool

is_bool2:                                         ; preds = %is_not_nil
  %raw_variant_pointer = getelementptr inbounds { [1 x i8], i8 }, ptr %tagged_value, i32 0, i32 1
  %bool_t_int = load i8, ptr %raw_variant_pointer, align 1
  %ret = icmp eq i8 %bool_t_int, 1
  ret i1 %ret

is_not_bool:                                      ; preds = %is_not_nil
  ret i1 true
}

define ptr @tvalue_new_nil() {
entry:
  %ret = alloca %enum.TValue, align 8
  %tag_ptr = getelementptr inbounds { i8, [8 x i8] }, ptr %ret, i32 0, i32 0
  store i8 0, ptr %tag_ptr, align 1
  ret ptr %ret
}

define ptr @tvalue_new_bool(i8 %0) {
entry:
  %ret = alloca %enum.TValue, align 8
  %tag_ptr = getelementptr inbounds { i8, [8 x i8] }, ptr %ret, i32 0, i32 0
  store i8 1, ptr %tag_ptr, align 1
  %data_ptr = getelementptr inbounds %tvalue_bool, ptr %ret, i32 0, i32 1
  store i8 %0, ptr %data_ptr, align 1
  ret ptr %ret
}

define ptr @tvalue_new_number() {
entry:
  %ret = alloca %enum.TValue, align 8
  %tag_ptr = getelementptr inbounds { i8, [8 x i8] }, ptr %ret, i32 0, i32 0
  store i8 2, ptr %tag_ptr, align 1
  ret ptr %ret
}

define ptr @tvalue_new_string(ptr %0) {
entry:
  %ret = alloca %enum.TValue, align 8
  %tag_ptr = getelementptr inbounds { i8, [8 x i8] }, ptr %ret, i32 0, i32 0
  store i8 3, ptr %tag_ptr, align 1
  %data_ptr = getelementptr inbounds %tvalue_string, ptr %ret, i32 0, i32 1
  store ptr %0, ptr %data_ptr, align 8
  ret ptr %ret
}

define ptr @new_byte_array(i64 %length, ptr %bytes) {
entry:
  %ret = alloca { i64, ptr }, align 8
  %len_ptr = getelementptr inbounds { i64, ptr }, ptr %ret, i32 0, i32 0
  store i64 %length, ptr %len_ptr, align 4
  %bytes_ptr = getelementptr inbounds { i64, ptr }, ptr %ret, i32 0, i32 1
  %loaded_ptr = load ptr, ptr %bytes_ptr, align 8
  store ptr %bytes, ptr %loaded_ptr, align 8
  ret ptr %ret
}

define i8 @test_nil() {
entry:
  %arg = call ptr @tvalue_new_nil()
  %ret = call i8 @getTValueTag(ptr %arg)
  ret i8 %ret
}

define i8 @test_bool() {
entry:
  %arg = call ptr @tvalue_new_bool(i1 true)
  %ret = call i8 @getTValueTag(ptr %arg)
  ret i8 %ret
}

define i8 @test_num() {
entry:
  %arg = call ptr @tvalue_new_number(float 0x41F0000000000000)
  %ret = call i8 @getTValueTag(ptr %arg)
  ret i8 %ret
}

define ptr @new_seeded_string() {
entry:
  %s_ptr = alloca [0 x i8], align 1
  store [11 x i8] c"hello world", ptr %s_ptr, align 1
  %ba = call ptr @new_byte_array(i8 11, ptr %s_ptr)
  %ret = call ptr @tvalue_new_string(ptr %ba)
  ret ptr %ret
}

define i8 @test_string() {
entry:
  %arg = call ptr @new_seeded_string()
  %ret = call i8 @getTValueTag(ptr %arg)
  ret i8 %ret
}

define i8 @main() {
entry:
  %ret = call i8 @test_string()
  %passed = icmp eq i8 %ret, 3
  br i1 %passed, label %passed1, label %failed
passed1:
  ret i8 0
failed:
  ret i8 1
}
