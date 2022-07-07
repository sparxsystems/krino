using Krino.ConstructiveGrammar.Dictionary;
using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.EnglishDictionary;
using Krino.EnglishGrammar.Parsing;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.ConstructiveGrammar.Tests.Dictionary
{
    [TestFixture]
    public class ConstructiveDictionaryTest
    {
        private IConstructiveDictionary myDictionary;

        [OneTimeSetUp]
        public void Setup()
        {
            //Trace.StartProfiler();

            myDictionary = new EnglishConstructiveDictionaryFactory().Create();

            //Trace.StopProfiler();
            //Thread.Sleep(300);
        }


        [Test]
        public void Parse_SimpleSentence()
        {
            var text = myDictionary.Parse("I read book.");

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
