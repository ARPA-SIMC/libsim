program example_char_utilities

use char_utilities

character (len=100) :: result

result=wash_char("/path/OTHERPATH/123-y-e....s")
print *,result 


end program example_char_utilities
