- title : A Developer's journey from OO to FP
- description : A Developer's journey from OO to FP
- author : Reid Evans
- theme : Simple
- transition : Fade

***

### A Developer's journey from OO to Functional
####[@ReidNEvans](http://twitter.com/reidnevans)
####http://reidevans.tech
####[@FunctionalKnox](http://twitter.com/functionalknox)

***

Overview

* OO design patterns
* SOLID principles
* Code Bloat
* Staying focused on the happy path
* Anecdotal Results

***

<section data-background="#5bc0de">

The model you use to view the world shapes the thoughts you are able to think.

**@TheBurningMonk**
</section>

***

My beginnings in professional development

	[lang=pascal]
	procedure TForm1.Button1Click(Sender: TObject);
	begin
		Label1.Caption := 'Hello World';
	end;

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
		CreateLocation();
	}
	
	void CreateLocation()
	{
		using (var con = new SqlConnection("MyDb"))
		using (var command = con.CreateCommand())
		{
			command.CommandText = 
				@"Insert into Locations ([State], City)
				  Values(@State, @City)";
				  
			command.Parameters.AddWithValue("State", tbState.Text);
			command.Parameters.AddWithValue("City", tbCity.Text);  
				  
			con.Open();
			command.ExecuteNonQuery();
		}
	}

' Procedural Programming
' this seems simple but moving to procedural code is a paradigm shift

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
	public class Bar
	{
		public IBeverage MakeDrink(Person person)
		{
			switch (person)
			{
				case Person.TheDude : return new Caucasian();
				case Person.JackieTreehorn : return new HellOfACaucasian();
				default: return new WhiteRussian();
			}
		}
	}
	
' here we return the abstraction

***

Strategy

	[lang=cs]
	interface IBowler
	{
		bool Rolls(DateTime date);
	}
	
	class WalterSobchak : IBowler
	{
		public bool Rolls(DateTime date)
		{
			return !date.IsSabbath;
		}
	}

' here we're dependent upon the abstraction to do work
	
***

<section data-background="#5bc0de">

Design patterns are just terms to define how an abstraction is passed around in code

' they are an answer, but not to the question I really had

</section>

***

<section data-background="#F0AD4E">

How do I know if the code I'm writing is any good?

</section>

***

## Solid principles

* Single Responsibility Principle
* Open Closed Principle
* Liskov Substitution Principle
* Interface Segregation Principle
* Dependency Inversion Principle

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

' now we adhere to SOLID principles
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
If you have a class with two methods and one of them is the constructor you have a function

**@JackDied**
</section>

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

' low signal to noise ratio
' important parts are 14-16, rest is plumbing

---

	let createLocation insert isValid city state =
		if state |> isValid
		then insert city state
		else raise (System.ArgumentException("Not a valid State"))
		
' createLocation function that accepts 4 args
' closures are the poor man's objects :)
' throwing an error isn't very functional.  We'll revisit

***

<section data-background="#F0AD4E">
### What is Functional Programming?
</section>

***

![DomainCodomain](images/domain-codomain.png)

---

## Totality

Every element in the domain must be mapped to some element in the codomain 

---

## Determinism

Calling a function with the same value (in domain) results in same value (in codomain).

***

	let double x = x * 2
	
---

	[lang=js]
	function greet(who) {
		console.log("hello " + who)
	}

---

	let formatDate format =
		DateTime.Now.ToString(format)

---

	[lang=cs]
	public string FormatDate(DateTime date, string format)
	{
		return date.ToString(format);
	}

---

	let divide x y = x / y
	
---

	let divide x y =
		if y = 0
		then None
		else Some (x / y)

***

<section data-background="#5bc0de">

Every function can be replaced with any other function that has the same signature

' any other function with the same signature can be substituted
</section>

***

	[lang=cs]
	interface ICalculator { int Calculate(int input); }

	//Decorator Pattern
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

---

Equivalent* JavaScript

	[lang=js]
	var log = function(func, input) {
		console.log(input);
		var result = func(input);
		console.log(result);
		return result;
	}

' no interface defined, no private ICalculator to manage
' no IOC container needed to manage dependencies
' passing functions rather than objects
' * => the log function works for any function
	
---

Statically checked types in C#

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

' and the knock on functional languages is that they have all these crazy symbols
' now statically checked

***

<section data-background="#5bc0de">
Staying focused on the happy path
</section>

***

Happy Path in C#

	[lang=cs]
	public void InsertLocation(Location location)
	{
		validateRequest(location);
		db.updateDbFromRequest(location);
		email.EmailNearbyCustomers(location);
	}
	
' what if the request isn't valid?

---

	[lang=cs]
	public void InsertLocation(Location location)
	{
		if (!validateRequest(location))
			throw new ArgumentException("Location not valid");
		db.updateDbFromRequest(location);
		email.EmailNearbyCustomers(location);
	}

' what if the db throws an error?

--- 

Unhappy Path in C#

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
			throw;
		}	
		email.EmailNearbyCustomers(location);
	}
	
' multiple returns
	
***

Happy Path in F#

	let insertLocation = 
		validateRequest
		>>= updateDbFromRequest
		>>= emailNearbyCustomers

---

Unhappy Path in F#

	let insertLocation = 
		validateRequest
		>>= updateDbFromRequest
		>>= emailNearbyCustomers

---

![Liar](images/liar.gif)
	
***

	[lang=js]
	var addOne = x => x + 1

	$.when(1)
	.then(addOne)
	.then(console.log);	
	// 2
	
' our functions aren't written to know about promises
' the then function does magic to allow us to work with the underlying structure

---

C# .NET 4.5
	
	[lang=cs]
	async Task<int> AddSomeNumbers()
	{
		int one = await Task.FromResult(1);
		int two = await Task.FromResult(2);
		return one + two;
	}
	
Javascript ES7
	
	[lang=js]
	async function getTweetContent() {
		let tweet = await getJSON('http://twitter.com/mytweet.json');
		return tweet.Content;
	}

---

	[lang=js]
	_.map([1,2,3], x => x + 2)
	// [3,4,5]

---

	[lang=js]
	_.map(['Hello', 'Nashville'], x => x.split())
	// [ ['H','e','l','l','o'], ['N','a','s','h','v','i','l','l','e'] ]
	
---

	[lang=js]
	_.flatMap(['Hello', 'Nashville'], x => x.split())
	// ['H','e','l','l','o','N','a','s','h','v','i','l','l','e']

***

<section data-background="#5bc0de">
Arrays, Promises, Options, etc are all just contexts for data.
</section>

***

Unhappy Path in F#

	let insertLocation = 
		validateRequest
		>>= updateDbFromRequest
		>>= emailNearbyCustomers
		
---

	let insertLocation = 
		validateRequest
		>> flatMap updateDbFromRequest
		>> flatMap emailNearbyCustomers

***

	type Option<'T> =
		| Some of 'T
		| None

	let divide bottom top =
		if bottom = 0
		then None
		else Some(top/bottom)
		
	divide 4 8
	//int option = Some 2
	
	divide 0 8
	//int option = None
	
' How do we chain them?
' divideBy takes ints and returns an int option

---

	divide 4 8
	|> Option.bind (divide 2)
	// int option = Some 1
	
	divide 0 8
	|> Option.bind (divide 2)
	// int option = None
	
---

	let square x = x * x

	divide 4 8
	|> Option.map square
	|> Option.bind (divide 2)
	// int option = Some 2

***

<section data-background="#F0AD4E">
How do I choose a functional language?
</section>

***

* **Clojure** - A Lisp on the JVM
* **Elm** - Pure language for the browser
* **Erlang / Elixir** - Massive scalability
* **F#** - Functional first on CLR / .NET
* **Haskell** - Pure functional language 
* **Scala** - Functional first on the JVM
* **Your Current Language**

***


## Anecdotal Results of switching to FP

* Social dealer management system for Freightliner
* 5 months development
* 3,582 lines of code
* 2 bugs not caught at compile time

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
		
***

<section data-background="#5bc0de">

The model you use to view the world shapes the thoughts you are able to think.

**@TheBurningMonk**
</section>

' If there were parts of this talk you didnt understand 

***

###[@ReidNEvans](http://twitter.com/reidnevans)

####http://reidevans.tech
####[@FunctionalKnox](http://twitter.com/functionalknox)
