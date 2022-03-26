using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Parsing
{
    [TestFixture]
    public class EnumBaseExtTest
    {
        [Test]
        public void GetGrammarId()
        {
            // Simple enum
            Assert.AreEqual("Morpheme.Free.Lexical.Noun", GrammarAttributes.Morpheme.Free.Lexical.Noun.GetGrammarId());
            Assert.AreEqual("Morpheme", GrammarAttributes.Morpheme.GetGrammarId());

            // BigInteger attributes.
            var attributes = GrammarAttributes.Morpheme.Free.Lexical.Noun | GrammarAttributes.Morpheme.Free.Lexical.Verb;
            Assert.AreEqual("Morpheme.Free.Lexical.(Noun,Verb)", attributes.GetGrammarId());

            // The root.
            Assert.AreEqual("", GrammarAttributes.Instance.GetGrammarId());
        }
    }
}
