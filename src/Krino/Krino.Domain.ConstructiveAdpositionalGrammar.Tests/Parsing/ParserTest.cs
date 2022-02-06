using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishDictionary;
using Krino.Domain.EnglishGrammar.Parsing;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Parsing
{
    [TestFixture]
    public class ParserTest
    {
        [Test]
        public void Parse_SimpleSentence()
        {
            var dictionary = new EnglishConstructiveDictionaryFactory().Create();
            var grammar = new EnglishMachine();
            var parser = new Parser(dictionary, grammar.Machine);

            var sentences = parser.Parse("I read book");
            
            Assert.AreEqual(1, sentences.Count);
            Assert.AreEqual("i read book", sentences[0].Value);
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
