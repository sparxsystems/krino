using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Attributes;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Morphemes.Attributes
{
    [TestFixture]
    public class MorphemeAttributesTest
    {
        [Test]
        public void Noun()
        {
            ulong attribute = MorphemeAttributes.Noun.Common.Concrete | MorphemeAttributes.Noun.Countable;

            Assert.IsTrue(MorphemeAttributes.Noun.Common.Concrete.IsIn(attribute));
            Assert.IsTrue(MorphemeAttributes.Noun.Countable.IsIn(attribute));
            Assert.IsFalse(MorphemeAttributes.Noun.Common.Collective.IsIn(attribute));
            Assert.IsFalse(MorphemeAttributes.Noun.UnCountable.IsIn(attribute));
        }
    }
}
