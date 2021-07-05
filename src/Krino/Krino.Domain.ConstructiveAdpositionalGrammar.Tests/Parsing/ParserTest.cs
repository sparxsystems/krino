using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishDictionary;
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
            var parser = new Parser(dictionary);

            var sentences = parser.Parse("I read book.");
            
            Assert.AreEqual(1, sentences.Count);
            Assert.AreEqual("i read book .", sentences[0].Value);
        }

        [Test]
        public void Parse_Argument()
        {
            var dictionary = new EnglishConstructiveDictionaryFactory().Create();
            var parser = new Parser(dictionary);

            var sentences = parser.Parse("Book is book because book is book");

            Assert.AreEqual(1, sentences.Count);
            Assert.AreEqual("cycling on the grass is prohibited because walking on the grass is prohibited .", sentences[0].Value);
        }
    }
}
