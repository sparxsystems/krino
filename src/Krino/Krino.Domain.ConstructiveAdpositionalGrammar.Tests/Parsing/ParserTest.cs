using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishDictionary;
using NUnit.Framework;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
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
                MaxWords = 8,
                RootPattern = EnglishPattern.e_Period_I,
            }.Create();
            var parser = new Parser(dictionary);

            var graph = dictionary.Patterns.CreatePatternGraph();
            var signatures = graph.GetAdTreeFactories(EnglishPattern.e_Period_I, 8)
                .Select(x => x.PatternSignature)
                .ToList();

            Assert.IsTrue(signatures.Contains("OIOUOIOU"));

            var sentences = parser.Parse("Book is book because book is book");

            Assert.AreEqual(1, sentences.Count);
            //Assert.AreEqual("cycling on the grass is prohibited because walking on the grass is prohibited .", sentences[0].Value);
        }
    }
}
