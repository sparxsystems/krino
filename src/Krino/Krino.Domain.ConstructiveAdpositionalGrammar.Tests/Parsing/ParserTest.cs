using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishDictionary;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using NUnit.Framework;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Parsing
{
    [TestFixture]
    public class ParserTest
    {
        [Test]
        public void Parse_SimpleSentence()
        {
            var dictionary = new EnglishConstructiveDictionaryFactory().Create();
            var parser = new Parser(dictionary);

            var sentences = parser.Parse("I read book.");
            
            Assert.AreEqual(1, sentences.Count);
            Assert.AreEqual("i read book .", sentences[0].Value);
        }

        [Test]
        public void Parse_Argument()
        {
            var dictionary = new EnglishConstructiveDictionaryFactory()
            {
                MaxWords = 7,
            }.Create();
            var parser = new Parser(dictionary);

            var sentences = parser.Parse("Book is book because book is book");

            Assert.AreEqual(1, sentences.Count);

            Assert.IsTrue(StructureAttributes.Sentence.Complex.IsIn(sentences[0].Attributes));

            Assert.IsTrue(StructureAttributes.Clause.Declarative.IsIn(sentences[0].Clauses[0].Attributes));
            Assert.IsTrue(StructureAttributes.Clause.Conclusion.IsIn(sentences[0].Clauses[0].Attributes));
            Assert.AreEqual("book", sentences[0].Clauses[0].Subject.Value);
            Assert.AreEqual("is book", sentences[0].Clauses[0].Predicate.Value);

            Assert.IsTrue(StructureAttributes.Clause.Declarative.IsIn(sentences[0].Clauses[1].Attributes));
            Assert.IsTrue(StructureAttributes.Clause.Premis.IsIn(sentences[0].Clauses[1].Attributes));
            Assert.AreEqual("book", sentences[0].Clauses[1].Subject.Value);
            Assert.AreEqual("is book", sentences[0].Clauses[1].Predicate.Value);

            //Assert.AreEqual("cycling on the grass is prohibited because walking on the grass is prohibited .", sentences[0].Value);
        }
    }
}
