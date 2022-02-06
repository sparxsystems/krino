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
            Assert.AreEqual("Morpheme.O.Free.Noun", GrammarAttributes.Morpheme.O.Free.Noun.GetGrammarId());
            Assert.AreEqual("Morpheme", GrammarAttributes.Morpheme.GetGrammarId());

            // The root.
            Assert.AreEqual("", GrammarAttributes.Instance.GetGrammarId());
        }
    }
}
