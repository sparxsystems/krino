using Antlr4.Runtime;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.EnglishDictionary;
using Krino.EnglishGrammar.Morphology;
using NUnit.Framework;
using System.Linq;

namespace Krino.EnglishGrammar.Tests
{
    [TestFixture]
    public class EnglishGrammarVisitorTest
    {
        [Test]
        public void Test()
        {
            var input = "adverb";
            var inputStream = new AntlrInputStream(input);
            var englishLexer = new EnglishLexer(inputStream);

            var commonTokenStream = new CommonTokenStream(englishLexer);
            var englishParser = new EnglishParser(commonTokenStream);

            var adverbContext = englishParser.adverbPhrase();

            var englishVisitor = new EnglishGrammarVisitor();
            var output = englishVisitor.Visit(adverbContext);
        }
    }
}
