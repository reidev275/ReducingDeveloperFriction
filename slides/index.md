- title : Reducing Developer Friction
- description : A Developer's journey from OO to FP
- author : Reid Evans
- theme : Simple
- transition : Fade

***

's' turns on speaker mode with dual screen

Learning a new paradigm is one of the more difficult things to do in software development. 
So why would an object oriented developer of 10 years suddenly decide to make the drastic switch to functional programming? 
In this talk I’ll show you why I started looking for other ways of writing software and why the switch wasn't as sudden or as drastic as it may seem.

We’ll start our journey with C#, discussing SOLID principals and the use of IoC containers. 
Then we’ll move to JavaScript to see first class functions and closures. 
Next we'll visit the exciting distributed world of Elixir on the Erlang VM. 
We'll finish up with F#, seeing type providers, discriminated unions, and maybe even a certain 5 letter M word.


***
## Reducing Developer Friction

#### A Developer’s Journey from OO to FP

<br>
<br>

###[@ReidNEvans](http://twitter.com/reidnevans)


' good morning
' thank you so much for coming today
' This is Reducing Developer Friction 
' A developer’s journey from OO to FP
' I’m Reid Evans and you can find me on Twitter @ReidNEvans
' developing since 95, pro in 04

***

![Tombras](images/tombras.jpg)

' I’m currently a Senior Developer at The Tombras Group
' INSERT TOMBRAS COPY HERE
' we're going to start off slow and move to 


***

My beginnings in professional development

	[lang=pascal]
	procedure TForm1.Button1Click(Sender: TObject);
	begin
		Label1.Caption := 'Hello World';
	end;

' hello world in Delphi
' ... which then leads us to complex data stuff in Delphi
	
***
	[lang=pascal]
	procedure TForm1.Button1Click(Sender: TObject);
	begin
		Locations := TTable.Create(Self);
		with Locations do
		begin
			DatabaseName := 'MyDB';
			TableName := 'MyTable';
			Open;
			
			Append;
			FieldByName('City').Value := cityEdit.Text;
			FieldByName('State').Value := stateEdit.Text;
			Post;
		
			Close;			
		end;
	end;
	
' in the UI we're opening a db connection to a table 
' we're appending, setting some values and saving
' 1 layer, very rapid development, very difficult maintenance
' so then I hear all this amazing stuff about .net
	
***

	[lang=cs]
	void button1_Click(object sender, System.EventArgs e)
	{
		using (var con = new SqlConnection("MyDb"))
		using (var command = con.CreateCommand())
		{
			command.CommandText = 
				@"Insert into Locations ([State], City)
				  Values(@State, @City)";
				  
			command.Parameters.AddWithValue("City", tbCity.Text);
			command.Parameters.AddWithValue("State", tbState.Text);  
				  
			con.Open();
			command.ExecuteNonQuery();
		}		
	}
	
' reduced LOC because of better api
' still Event Driven Programming
	
	
***

<section data-background="#F0AD4E">
What if I want to do the same thing from a different event?
</section>

***

	[lang=cs]
	void button1_Click(object sender, System.EventArgs e)
	{
		CreateLocation(tbCity.Text, tbState.Text);
	}
	
	void CreateLocation(string city, string state)
	{
		using (var con = new SqlConnection("MyDb"))
		using (var command = con.CreateCommand())
		{
			command.CommandText = 
				@"Insert into Locations ([State], City)
				  Values(@State, @City)";
				  
			command.Parameters.AddWithValue("State", state);
			command.Parameters.AddWithValue("City", city);  
				  
			con.Open();
			command.ExecuteNonQuery();
		}
	}

' Procedural Programming
' this seems simple but moving to procedural code is a paradigm shift
' CreateLocation can be placed anywhere now.  It is no longer tied to the form

***

#Solid principals

' all images courtesy of Derrick Bailey 

***

### Single Responsibility Principal

A method/class/function should have only one reason to change

---

![Single Responsibility Principal](images/srp.jpg)


***

### Open Closed Principal

Methods/classes/functions should be open for extension but closed for modification

---

![Open Closed Principal](images/ocp.jpg)

***

### Liskov Substitution Principal

Parent types should be substitutable by their child types

---

![Liskov Substitution Principal](images/lsp.jpg)

***

### Interface Segregation Principal 

No client should be forced to depend on methods it does not use

---

![Interface Segregation Principal](images/isp.jpg)

***

### Dependency Inversion Principal

High level modules should not depend on low level modules

---

![Dependency Inversion Principal](images/dip.jpg)

***

<section data-background="#F0AD4E">
Can't I do all of that in C#?
</section>
	
***	
	
	
	public class LocationsController : ApiController {
		public void Post(string city, string state) 
		{
			CreateLocation(city, state);
		}
		
		void CreateLocation(string city, string state) 
		{
			using (var con = new SqlConnection("MyDb"))
			using (var command = con.CreateCommand()) {
				command.CommandText = 
					@"Insert into Locations ([State], City)
					  Values(@State, @City)";
					  
				command.Parameters.AddWithValue("State", state);
				command.Parameters.AddWithValue("City", city);  
					  
				con.Open();
				command.ExecuteNonQuery();
			}
		}
	}

' example from earlier moved into web api

***

	public interface ILocationRepository
	{
		void Insert(string city, string state);
	}

	public class LocationsController : ApiController 
	{
		readonly ILocationRepository _locationRepository;
		
		public LocationsController(ILocationRepository locationRepository)
		{
			_locationRepository = locationRepository;
		}
	
		public void Post(string city, string state)
		{
			_locationRepository.Insert(city, state);
		}
	}
	
' Constructor Injection DI version
' SOLID, if contrived example
' should probably null check in the constructor
' realistically there's probably a business layer for validation etc.

***




An object fundamentally has two reasons to change because it contains data and methods



***

First class functions

---

JavaScript

***

<section data-background="#F0AD4E">
Can't I do all of that in C#?
</section>

' I was stuck here for about a year and a half
' lets look into how I had been coding

***

' DI code with Test Induced Damage
' Mark Seeman's Look no mocks talk`

***

> "If you have a class with 2 methods and one of them is init, you probably have a function" @jackdied Jack Dietrich

***

In F#, the type signature for a function is equivalent to an interface

' all functions with the same signature are substitutable
' functions can only act on arguments, not built in state
' immediately removes half of the code we have to write and maintain
' no need for interfaces.  no need for IOC containers.

***

Railway Oriented Programming
or the Maybe monad

***

<section data-background="#F0AD4E">
If F# can do Monads, why the fuss over Haskell?
</section>

***

Monad in F#
	
	type MaybeBuilder() =
		member this.Return(x) = Some x
		member this.Bind(x, f) = 
			match x with
			| None -> None
			| Some a -> f a
	
Monad in Haskell
	
	[lang=haskell]
	class Monad m where {
	  (>>=)  :: m a -> (a -> m b) -> m b
	  return :: a                 -> m a
	} 	
	
' the Fsharp example is for a specific monad
' the Haskell version is generic and accepts any monad
' not generic type classes like Haskell

***

Equivalent Haskell Monad in F#

	type Monad<'M> =
		abstract member bind : 'M<'a> -> ('a -> 'M<'b>) -> 'M<'b>
		abstract member ``return`` : 'a -> 'M<'a>

	//error FS0712: Type parameter cannot be used as type constructor

***

Status Quo
http://www.daedtech.com/tag/expert-beginner > @DaedTech

***

> "The model you use to view the world shapes the thoughts you are able to think." @theburningmonk



http://tinyurl.com/MonadsInPictures

Look, No Mocks! Functional TDD with F#
http://www.infoq.com/presentations/mock-fsharp-tdd
