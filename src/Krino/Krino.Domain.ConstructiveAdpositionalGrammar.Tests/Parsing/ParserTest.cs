using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishDictionary;
using Krino.Domain.EnglishGrammar.Parsing;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Parsing
{
    [TestFixture]
    public class ParserTest
    {
        private IConstructiveDictionary2 myDictionary;
        private EnglishMachine myGrammar;

        [OneTimeSetUp]
        public void Setup()
        {
            //Trace.StartProfiler();

            myDictionary = new EnglishConstructiveDictionaryFactory().Create();
            myGrammar = new EnglishMachine(true);

            //Trace.StopProfiler();
            //Thread.Sleep(300);
        }

        [TearDown]
        public void TearDown()
        {
            myGrammar.Machine.Reset();
        }

        [Test]
        public void Parse_SimpleSentence()
        {
            var parser = new Parser(myDictionary, myGrammar.Machine);

            var text = parser.Parse("I read book.");
            
            Assert.AreEqual(1, text.Count);
            Assert.AreEqual("i read book.", text[0].Value);
        }

        [Test]
        public void Parse_Argument()
        {
            //var dictionary = new EnglishConstructiveDictionaryFactory()
            //{
            //    MaxWords = 7,
            //}.Create();
            //var parser = new Parser(dictionary);

            //var sentences = parser.Parse("Book is book because book is book");

            //Assert.AreEqual(1, sentences.Count);

            //Assert.IsTrue(RootAttributes.Sentence.Complex.IsIn(sentences[0].Attributes));

            //Assert.IsTrue(RootAttributes.Clause.Declarative.IsIn(sentences[0].Clauses[0].Attributes));
            //Assert.IsTrue(RootAttributes.Clause.Conclusion.IsIn(sentences[0].Clauses[0].Attributes));
            //Assert.AreEqual("book", sentences[0].Clauses[0].Subject.Value);
            //Assert.AreEqual("is book", sentences[0].Clauses[0].Predicate.Value);

            //Assert.IsTrue(RootAttributes.Clause.Declarative.IsIn(sentences[0].Clauses[1].Attributes));
            //Assert.IsTrue(RootAttributes.Clause.Premis.IsIn(sentences[0].Clauses[1].Attributes));
            //Assert.AreEqual("book", sentences[0].Clauses[1].Subject.Value);
            //Assert.AreEqual("is book", sentences[0].Clauses[1].Predicate.Value);

            ////Assert.AreEqual("cycling on the grass is prohibited because walking on the grass is prohibited .", sentences[0].Value);
        }
    }
}
