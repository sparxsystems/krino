using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributes;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Morphemes.StructuralAttributes
{
    [TestFixture]
    public class StructAttributesTest
    {
        [Test]
        public void Noun()
        {
            ulong attribute = StructAttributes.Noun.Common.Concrete | StructAttributes.Noun.Countable;

            Assert.IsTrue(StructAttributes.Noun.Common.Concrete.IsIn(attribute));
            Assert.IsTrue(StructAttributes.Noun.Countable.IsIn(attribute));
            Assert.IsFalse(StructAttributes.Noun.Common.Collective.IsIn(attribute));
            Assert.IsFalse(StructAttributes.Noun.UnCountable.IsIn(attribute));
        }
    }
}
