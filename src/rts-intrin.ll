declare double @llvm.floor.f64(double)

define double @floor(double %value) alwaysinline {
  %1 = call double @llvm.floor.f64(double %value)
  ret double %1
}
