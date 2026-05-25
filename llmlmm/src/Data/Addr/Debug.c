#include <stdio.h>

void debug_allocAddrBytesPrimOp(size_t n, void *p) {
    fprintf(stderr, "non-GC heap: %p <- allocAddrBytesPrimOp(%zu)\n", p, n);
}

void debug_allocAddrBytesAlignedPrimOp(size_t n, size_t d, void *p) {
    fprintf(stderr, "non-GC heap: %p <- allocAddrBytesAlignedPrimOp(%zu, %zu)\n", p, n, d);
}

void debug_callocAddrBytesPrimOp(size_t n, void *p) {
    fprintf(stderr, "non-GC heap: %p <- callocAddrBytesPrimOp(%zu)\n", p, n);
}

void debug_callocAddrBytesAlignedPrimOp(size_t n, size_t d, void *p) {
    fprintf(stderr, "non-GC heap: %p <- callocAddrBytesAlignedPrimOp(%zu, %zu)\n", p, n, d);
}

void debug_reallocAddrBytesPrimOp(void *p, size_t n, void *q) {
    fprintf(stderr, "non-GC heap: %p <- reallocAddrBytesPrimOp(%p, %zu)\n", q, p, n);
}

void debug_freeAddrPrimOp(void *p) {
    fprintf(stderr, "non-GC heap: !_ <- freeAddrPrimOp(%p)\n", p);   
}

void debug_setAddrBytesPrimOp (void *p, size_t n, char c) {
    fprintf(stderr, "non-GC heap: !_ <- setAddrBytesPrimOp(%p, %zu, %hhu)\n", p, n, c);
}

void debug_copyAddrBytesPrimOp(void *p, void *q, size_t n) {
    fprintf(stderr, "non-GC heap: !_ <- copyAddrBytesPrimOp(%p, %p, %zu)\n", p, q, n);
}

void debug_copyAddrNonOverlappingBytesPrimOp(void *p, void *q, size_t n) {
    fprintf(stderr, "non-GC heap: !_ <- copyAddrNonOverlappingBytesPrimOp(%p, %p, %zu)\n", p, q, n);
}