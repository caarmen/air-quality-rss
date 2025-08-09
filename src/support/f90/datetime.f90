module datetime
   implicit none
contains
   function get_today_str() result(today_str)
      character(len=8) :: today_str
      integer, dimension(8) :: today_values
      call date_and_time(values=today_values)
      write (today_str, '(I4.4,I2.2,I2.2)') today_values(1), today_values(2), today_values(3)
   end function get_today_str

   function get_hyphenated_today_str() result(today_str)
      character(len=10) :: today_str
      integer, dimension(10) :: today_values
      call date_and_time(values=today_values)
      write (today_str, '(I4.4,"-",I2.2,"-",I2.2)') today_values(1), today_values(2), today_values(3)
   end function get_hyphenated_today_str

   function get_yesterday_str() result(today_str)
      character(len=8) :: today_str
      integer, dimension(8) :: today_values
      call date_and_time(values=today_values)
      write (today_str, '(I4.4,I2.2,I2.2)') today_values(1), today_values(2), today_values(3) - 1
   end function get_yesterday_str
end module datetime
