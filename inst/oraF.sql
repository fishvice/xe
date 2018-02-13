CREATE OR REPLACE
FUNCTION geoconvert2(x number) RETURN number IS
  i number;
  p1 number;
  p2 number;
  p3 number;
  tmp number;
  lat number;

  BEGIN
    i := sign(x);
    lat := abs(x);
    p1 := floor(lat);
    p2 := floor((lat - p1)*60);
    p3 := floor((lat - p1 -p2/60)*100*60);
    tmp := i * (p1 * 10000 + p2 * 100 + p3);

    RETURN tmp;
  END geoconvert2;
/

CREATE OR REPLACE
FUNCTION geoconvert1(y number) RETURN number IS
  i number;
  x1 number;
  tmp number;
  x number;

  BEGIN
    i := sign(y);
    x := abs(y);
    x1 := mod(x,10000);
    tmp := (x/100)-trunc(x/10000)*100;
    tmp := (i*(x+(200/3)*tmp))/10000;
    RETURN tmp;
  END geoconvert1;
/
