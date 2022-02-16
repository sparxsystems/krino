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
            Assert.AreEqual("Morpheme.O.Free.Noun", GrammarAttributes.Morpheme.O.Free.Noun.GetGrammarId());
            Assert.AreEqual("Morpheme", GrammarAttributes.Morpheme.GetGrammarId());

            // BigInteger attributes.
            var attributes = GrammarAttributes.Morpheme.O.Free.Noun | GrammarAttributes.Morpheme.I.Free.Verb;
            Assert.AreEqual("Morpheme.(O.Free.Noun,I.Free.Verb)", attributes.GetGrammarId());

            // The root.
            Assert.AreEqual("", GrammarAttributes.Instance.GetGrammarId());
        }
    }
}
