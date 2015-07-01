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


#Solid principals

' all images courtesy of Derrick Bailey 

***

### Single Responsibility Principal

A method/class/function should have only one reason to change

***

![Single Responsibility Principal](images/srp.jpg)


***

### Open Closed Principal

Methods/classes/functions should be open for extension but closed for modification

***

![Open Closed Principal](images/ocp.jpg)

***

### Liskov Substitution Principal

Parent types should be substitutable by their child types

***

![Liskov Substitution Principal](images/lsp.jpg)

***

### Interface Segregation Principal 

No client should be forced to depend on methods it does not use

***

![Interface Segregation Principal](images/isp.jpg)

***

### Dependency Inversion Principal

High level modules should not depend on low level modules

***

![Dependency Inversion Principal](images/dip.jpg)


***

<section data-background="#F0AD4E">
Can't I do all of that in C#?
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
	
' now we adhere to SOLID principals
' I gave a presentation on this last year
' we can test / mock / stub
' go to definition now takes us to the interface
' have 3x lines of code

***

Code Complete by Steve McConnell

> **Industry Average**: about 15 - 50 errors per 1000 lines of delivered code.

<br>

> **Microsoft Applications**: about 10 - 20 defects per 1000 lines of code during in-house testing

***

<section data-background="#F0AD4E">
Is there a way to do this with fewer lines of code?
</section>

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

Equivalent JavaScript

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
' function is a first class citizen
' * => the log function works for any function

***

A closer C# equivalent

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

## Staying focused on the happy path

***

Happy Path in C#

	[lang=cs]
	public string InsertLocation(Location location)
	{
		validateRequest(location);
		db.updateDbFromRequest(location);
		return "Success";
	}
	
---

Non Happy Path in C#

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
	
' multiple returns
	
***

Happy Path in F#

	let executeUseCase = 
		validateRequest
		>> bind updateDbFromRequest
		>> bind emailNearbyCustomers

---

Unhappy Path in F#

	let executeUseCase = 
		validateRequest
		>> bind updateDbFromRequest
		>> bind emailNearbyCustomers

***

<section data-background="#F0AD4E">
How is that possible?
</section>	
	
***

Railway Oriented Programming
or the Maybe monad

***

* LINQ
* Async / Await
* Lodash / Underscore
* Promises

' who has used one of these? 
' anyone not used one of these?
' all examples of Monads

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

Status Quo
http://www.daedtech.com/tag/expert-beginner > @DaedTech

***

> "The model you use to view the world shapes the thoughts you are able to think." @theburningmonk



http://tinyurl.com/MonadsInPictures

Look, No Mocks! Functional TDD with F#
http://www.infoq.com/presentations/mock-fsharp-tdd
