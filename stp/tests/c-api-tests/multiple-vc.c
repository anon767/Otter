#include <stdio.h>
#include "c_interface.h"

int main() {
  VC vc1 = vc_createValidityChecker();
  VC vc2 = vc_createValidityChecker();

  vc_Destroy(vc1);
  vc_Destroy(vc2);
}
