/* Examples for testing */
class Foo<X extends Object, Y extends Object> extends Object {
  Object f1;
  Foo<X,Y> m(Object arg, Object arg2){ 
      return this.id(new Bar<X,Y>(f1=new Object(),f2=this)); 
  };

  Foo<X,Y> id( Foo<X,Y> x ){
    return x.f2;
  };
};

class Bar< X extends Object, Y extends Object > extends Foo<X,Y> {
  Foo<X,Y> f2;
};

/* this works without type checking */ 
(new Bar(f2=new Object())).f2;

/* this works without type checking */ 
(new Bar<Object,Object>(f1=new Object(), f2=new Foo<Object,Object>(f1= new Object())
)).f2;

/* this always gets stuck */ 
(new Bar()).f2;

/* this always  gets stuck */ 
(new Bar()).f3;

/* run the first expression */
(new Bar<Object,Object>(f1=new Object(), f2=new Foo<Object,Object>(f1=new
Object()))).
m(new Object(), new Object());
