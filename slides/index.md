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
' Tombras is a 140+ employee, full-service agency with a digital mindset. 
' We are one of the top 35 independent agencies in north america. 

***

Don't be an Expert Beginner

> Someone should write a utility that searches your code base to make sure all the code you copy and paste stays in sync.

http://www.daedtech.com/tag/expert-beginner > @DaedTech

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
' anyone had any experience with Delphi?  Anders Hejlsberg's work pre msft and C#
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
				  
			command.Parameters.AddWithValue("City", textboxCity.Text);
			command.Parameters.AddWithValue("State", textboxState.Text);  
				  
			con.Open();
			command.ExecuteNonQuery();
		}		
	}
	
' reduced LOC because of better api - SQL
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
	
	[lang=cs]
	public class LocationsManager  
	{
		public void CreateLocation(string city, string state) 
		{
			if (!States.Contains(state)) throw new ArgumentException()
			
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

' still procedural / imperative but no longer has any ties to the UI
' data validation and data persistence in a single method

***

<section data-background="#F0AD4E">
How should all this code fit together?
</section>

***

### OO Design patterns

***

Factory 

	[lang=cs]
	public class Factory
	{
		public IFoo GetImplementation(MyEnum enum)
		{
			switch (enum)
			{
				case MyEnum.Foo : return new Foo();
				case MyEnum.Bar : return new Bar();
				default: return null;
			}
		}
	}
	
' here we return the abstraction

***

Strategy

	[lang=cs]
	public class LocationsManager  
	{
		readonly ILocationRepository _locationRepository;
		
		public LocationsManager(ILocationRepository locationRepository)
		{
			_locationRepository = locationRepository;
		}
	
		public void CreateLocation(string city, string state)
		{
			_locationRepository.Insert(city, state);
		}
	}

' here we're dependent upon the abstraction to do work
	
***
	
Command

	[lang=cs]
	public interface ICommand
	{
		void Execute();
	}
	
' here the whole thing is an abstraction
		
***

<section data-background="#5bc0de">

Design patterns are just terms to define how an abstraction is passed around in code

' they are an answer, but not to the question I really had

</section>

***

<section data-background="#F0AD4E">

How do I know if the code I'm writing is any good?

</section>

' you can write really bad code using really good patterns and vice versa

***

## Solid principals

***

### Single Responsibility Principal

' A method/class/function should have only one reason to change

***

![Single Responsibility Principal](images/srp.jpg)

***

### Open Closed Principal

' Methods/classes/functions should be open for extension but closed for modification

***

![Open Closed Principal](images/ocp.jpg)

***

### Liskov Substitution Principal

' Parent types should be substitutable by their child types

***

![Liskov Substitution Principal](images/lsp.jpg)

***

### Interface Segregation Principal 

' No client should be forced to depend on methods it does not use

***

![Interface Segregation Principal](images/isp.jpg)

***

### Dependency Inversion Principal

' High level modules should not depend on low level modules

***

![Dependency Inversion Principal](images/dip.jpg)

***

<section data-background="#F0AD4E">
Can't I do all of that in Object Oriented code?
</section>

***

	[lang=cs]
	public class LocationsManager  
	{
		public void CreateLocation(string city, string state) 
		{
			if (!States.Contains(state)) throw new ArgumentException()
			
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

' locationsManager from earlier

***

	[lang=cs]
	public class LocationsManager  
	{
		readonly ILocationRepository _locationRepository;
		readonly IStateValidator _stateValidator;
		
		public LocationsManager(ILocationRepository r, IStateValidator v)
		{
			_locationRepository = r; 
			_stateValidator = v;
		}
	
		public void CreateLocation(string city, string state)
		{
			if (!_stateValidator.IsValid(state)) 
				throw new ArgumentException("Not a valid State");
			_locationRepository.Insert(city, state);
		}
	}
	
' Constructor Injection DI version
' multiple strategy patterns

---

	[lang=cs]
	public interface ILocationRepository
	{
		void Insert(string city, string state);
	}
	
	public class LocationRepository : ILocationRepository
	{
		public void Insert(string city, string state)
		{
			using (var con = new SqlConnection(Global.ConnectionString))
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
	
---

	[lang=cs]
	public interface IStateValidator
	{
		bool IsValid(string state);
	}
	
	public class StateValidator : IStateValidator
	{
		static readonly List<string> ValidStates = new List<string>
		{
			"AL", "AK", "AZ" //etc..
		};
		
		public bool IsValid(string state)
		{
			return ValidStates.Contains(state);
		}
	}
	
	
	
' we also need these interfaces and at least 1 implementation
' then we need some way of composing all of this

---

	[lang=cs]
	public class ServiceModule : NinjectModule
	{
		public override void Load()
		{
			Bind<ILocationRepository>().To<LocationRepository>();
				
			Bind<IStateValidator>().To<StateValidator>();
		}
	}
	
---

	[lang=cs]
	public class LocationsManager  
	{
		readonly ILocationRepository _locationRepository;
		readonly IStateValidator _stateValidator;
		
		public LocationsManager(ILocationRepository r, IStateValidator v)
		{
			_locationRepository = r; 
			_stateValidator = v;
		}
	
		public void CreateLocation(string city, string state)
		{
			if (!_stateValidator.IsValid(state)) 
				throw new ArgumentException("Not a valid State");
			_locationRepository.Insert(city, state);
		}
	}

' I gave a presentation on this last year	
' now we adhere to SOLID principals
' we can test / mock / stub
' BUT
' go to definition now takes us to the interface
' have 3x lines of code and do the same work

***

Code Complete by Steve McConnell

> **Industry Average**: about 15 - 50 errors per 1000 lines of delivered code.

<br>

> **Microsoft Applications**: about 10 - 20 defects per 1000 lines of code during in-house testing

***

<section data-background="#F0AD4E">
Can we stay SOLID without code bloat?
</section>

' Mark Seeman's Look No Mocks - Test Induced Damage

***

<section data-background="#5bc0de">

In functional languages, all functions are interfaces

' any other function with the same signature can be substituted
</section>

***

	[lang=cs]
	interface ICalculator { int Calculate(int input); }

	class AddingCalculator: ICalculator
	{
	   public int Calculate(int input) { return input + 1; }
	}

	class LoggingCalculator: ICalculator
	{
	   ICalculator _innerCalculator;
	   LoggingCalculator(ICalculator innerCalculator)
	   {
		  _innerCalculator = innerCalculator;
	   }

	   public int Calculate(int input) 
	   { 
		  Console.WriteLine("input is {0}", input);
		  var result  = _innerCalculator.Calculate(input);
		  Console.WriteLine("result is {0}", result);
		  return result; 
	   }
	}

' Calculator with a strategy pattern and a decorator

*** 

Equivalent* JavaScript

	[lang=js]
	var addingCalculator = function(input) { return input + 1 }
	
	var log = function(func, input) {
		console.log(input);
		let result = func(input);
		console.log(result);
		return result;
	}

' no interface defined, no private ICalculator to manage
' no IOC container needed to manage dependencies
' passing functions rather than objects
' * => the log function works for any function

***

A more accurate C# equivalent

	[lang=cs]
	public static TResult Log<T, TResult>(Func<T, TResult> func, T input)
	{
		Console.WriteLine("input is " + input.ToString());
		var result = func(input);
		Console.WriteLine("result is " + result.ToString());
		return result;
	}
	
' and the knock on functional languages is that they have all these crazy symbols
' benefit is that we're guaranteed that the input will fit with the func

***

<section data-background="#5bc0de">

Functional languages are generic by default

</section>

***

	[lang=cs]
	public static TResult Log<T, TResult>(Func<T, TResult> func, T input)
	{
		Console.WriteLine("input is " + input.ToString());
		var result = func(input);
		Console.WriteLine("result is " + result.ToString());
		return result;
	}


The same code in F#

	let log func input = 
		printfn "input is %A" input
		let result = func input
		printfn "result is %A" result
		result

' now statically checked
		
***

<section data-background="#F0AD4E">
Ok, less code is cool, but I need more than that to learn a new paradigm.
</section>

***

## Domain Driven Design
	
' I was not a fan until switching to the functional way
' doesn't have to mean fat domain models

***

	module CardGame = 
		type Suit = Club | Diamond | Spade | Heart
		
		type Rank = Two | Three | Four | Five | Six | Seven | Eight
							| Nine | Ten | Jack | Queen | King | Ace
						
		type Card = Suit * Rank
		
		type Hand = Card list
		type Deck = Card list
		
		type Player = { Name: string; Hand: Hand }
		type Game = { Deck: Deck; Players: Player list }
		
		type Deal = Deck -> (Deck * Card)
		type PickupCard = (Hand * Card) -> Hand
		

http://fsharpforfunandprofit/ddd

' make illegal states unrepresentable
		
*** 


## Staying focused on the happy path

***

Happy Path in C#

	[lang=cs]
	public string InsertLocation(Location location)
	{
		validateRequest(location);
		db.updateDbFromRequest(location);
		email.EmailNearbyCustomers(location);
		return "Success";
	}
	
' what if the request isn't valid?

---

	[lang=cs]
	public string InsertLocation(Location location)
	{
		if (!validateRequest(location))
			throw new ArgumentException("Location not valid");
		db.updateDbFromRequest(location);
		email.EmailNearbyCustomers(location);
		return "Success";
	}

' what if the db throws an error?

--- 

	[lang=cs]
	public void InsertLocation(Location location)
	{
		if (!validateRequest(location))
			throw new ArgumentException("Location not valid");
		try
		{
			db.updateDbFromRequest(location);
		} 
		catch (Exception e)
		{
			Logger.Log(e);
			return "Failure";
		}	
		email.EmailNearbyCustomers(location);
		return "Success";
	}
	
---	

Unhappy Path in C#

	[lang=cs]
	public string InsertLocation(Location location)
	{
		if (!validateRequest(location))
			return "Failure";
		try
		{
			db.updateDbFromRequest(location);
		} 
		catch (Exception e)
		{
			Logger.Log(e);
			return "Failure";
		}	
		email.EmailNearbyCustomers(location);
		return "Success";
	}
	
' multiple returns
	
***

Happy Path in F#

	let executeUseCase = 
		validateRequest
		>>= updateDbFromRequest
		>>= emailNearbyCustomers

---

Unhappy Path in F#

	let executeUseCase = 
		validateRequest
		>>= updateDbFromRequest
		>>= emailNearbyCustomers

***

<section data-background="#F0AD4E">
How is that possible?
</section>	
	
***

	[lang=js]
	$.when([1,2,3])
	.then(function (data) {
		console.log(data)
	});
	// [1,2,3]
	
' can replace when with ajax

---

	function log (x) { console.log(x) }
	function addOne (x) { return x + 1 }

	[lang=js]
	$.when([1,2,3])
	.then(function (data) {
		return data.map(addOne)
	}).then(log);	
	// [2,3,4]
	
' our functions aren't written to know about promises
' then does magic to allow us to work with the underlying structure

***

	

	type Option<'T> =
		| Some of 'T
		| None

' 

***

Railway Oriented Programming
or the Maybe monad

***

Maybe monad in F#

	type MaybeBuilder() =
		member this.Return(x) = Some x
		member this.Bind(x, f) = 
			match x with
			| None -> None
			| Some a -> f a

' live coding MaybeMonad.fsx
			
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

<section data-background="#F0AD4E">
How do I choose a functional language?
</section>

***

* **Haskell** - Pure language
* **Clojure** - A Lisp on JVM
* **F#** - Functional first on CLR / .NET
* **Erlang / Elixir** - Massive scalability


' all languages have pros and console
' need 'web scale'? look at Erlang and Elixir
' already on .Net? look at F# though Clojure can target clr
' already on Java? Clojure and Scala
' want a pure language? try Haskell

***

F# Type Providers

***

	[lang=cs]
	public void Insert(string city, string state)
	{
		using (var con = new SqlConnection(Global.ConnectionString))
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

' using ado
' it would have taken me a test to know State required the brackets
	
***

	type LocationInsert = 
		SqlCommandProvider<
			"INSERT INTO Locations([State], City)
			VALUES (@State, @City)", "connectionString">
	
	let insertLocation city state =
		use command = new LocationInsert()
		command.Execute(state, city)
		
' sql is checked at design time against actual db
' Execute forces correct arguments
' view live code demo

***

## Anecdotal Results of switching to FP

* Social dealer management system for Freightliner
* 2 months development
* 2,000 lines of code
* 2 bugs not caught at design / compile time

---

### Bug #1

	type GetChildObjects = 
		SqlCommandProvider<
			"Select * FROM ChildTable where id = @id", "connectionString">
	
	type GetChildObjects' = 
		SqlCommandProvider<
			"Select * FROM ChildTable where parentId = @id", "connectionString">	
			
' both require an int, no way for compiler to know I goofed

---

### Bug #2

	type GetChildObjects = 
		SqlCommandProvider<
			"Select * FROM ChildTable where parentId = @id", "connectionString">
			
	let getChildObjects parentId = 
		use query = new GetChildObjects()
		query.Execute(parentId)
		|> Seq.map (fun x -> { Id = x.Id; Name = x.Name })

' Too many open commands 
' Seq is lazy evaluated so by returning it the runtime wasn't sure when to dispose of the query

---

	type GetChildObjects = 
		SqlCommandProvider<
			"Select * FROM ChildTable where parentId = @id", "connectionString">
			
	let getChildObjects parentId = 
		use query = new GetChildObjects()
		query.Execute(parentId)
		|> Seq.map (fun x -> { Id = x.Id; Name = x.Name })
		|> Seq.toList

		
***

> **@theburningmonk** The model you use to view the world shapes the thoughts you are able to think.

http://fsharpforfunandprofit.com

Monads in pictures
http://tinyurl.com/MonadsInPictures

Look, No Mocks! Functional TDD with F#
http://www.infoq.com/presentations/mock-fsharp-tdd

http://theburningmonk.com/2015/04/dont-learn-a-syntax-learn-to-change-the-way-you-think/
