program example_char_utilities

use char_utilities

character (len=100) :: result
character(len=20) :: string
character(len=20) :: pattern

result=wash_char("/path/OTHERPATH/123-y-e....s")
print *,result 
print *,"--------------------------------------"


string  = 'abcdefghijk' ; pattern = '?b*'
write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
 match(string,pattern)

string  = 'abcdefghijk' ; pattern = '*c*'
write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
 match(string,pattern)

string  = 'abcdefghijk' ; pattern = '*c'
write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
 match(string,pattern)

string  = 'abcdefghijk' ; pattern = '*c*k'
write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
 match(string,pattern)

string  = 'LS' ; pattern = '?OW'
write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
 match(string(1:2),pattern)

string  = 'teztit' ; pattern = 'tez*t*t'
write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
 match(string,pattern)

!
! Two pattern match problems that might pose difficulties
!
string  = 'abcde ' ; pattern = '*e *'
write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
 match(string(1:6),pattern)

string  = 'baaaaa' ; pattern = 'b*a'
write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
 match(string,pattern)

string  = 'bababa' ; pattern = 'b*ba'
write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
 match(string,pattern)

string  = 'baaaaax' ; pattern = 'b*ax'
write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
 match(string,pattern)

string  = 'baaaaa' ; pattern = 'b*ax'
write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
 match(string,pattern)

string  = 'baaaaax' ; pattern = 'b*a'
write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
 match(string,pattern)

string  = '' ; pattern = 'b*'
write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
 match(string,pattern)

string  = '3' ; pattern = '??'
write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
 match(string(1:1),pattern)


end program example_char_utilities


