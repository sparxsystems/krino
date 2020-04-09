using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.AdTrees
{
    [TestFixture]
    public class AdTreeTest
    {
        [Test]
        public void Governor()
        {
            AdTree adTree = new AdTree();

            Assert.IsNull(adTree.Governor);

            AdTree governor = new AdTree();
            
            // Attach the governor.
            adTree.Governor = governor;
            Assert.AreEqual(governor, adTree.Governor);
            Assert.AreEqual(adTree, governor.AdPosition);

            // Detach the governor.
            adTree.Governor = null;
            Assert.IsNull(adTree.Governor);
            Assert.IsNull(governor.AdPosition);
        }

        [Test]
        public void Dependent()
        {
            AdTree adTree = new AdTree();

            Assert.IsNull(adTree.Dependent);

            AdTree dependent = new AdTree();

            // Attach dependent.
            adTree.Dependent = dependent;
            Assert.AreEqual(dependent, adTree.Dependent);
            Assert.AreEqual(adTree, dependent.AdPosition);

            // Detach the dependent.
            adTree.Dependent = null;
            Assert.IsNull(adTree.Dependent);
            Assert.IsNull(dependent.AdPosition);
        }

        [Test]
        public void ElementType()
        {
            AdTree adTree = new AdTree()
            {
                Governor = new AdTree()
                {
                    Governor = new AdTree()

                    // Dependent is missing per purpose.
                },
                Dependent = new AdTree()
                {
                    // Governor is missing per purpose.

                    Dependent = new AdTree()
                }
            };

            Assert.AreEqual(AdTreeElementType.AdPosition, adTree.ElementType);
            
            Assert.AreEqual(AdTreeElementType.AdPosition | AdTreeElementType.Governor, adTree.Governor.ElementType);
            Assert.AreEqual(AdTreeElementType.Governor, adTree.Governor.Governor.ElementType);

            Assert.AreEqual(AdTreeElementType.AdPosition | AdTreeElementType.Dependent, adTree.Dependent.ElementType);
            Assert.AreEqual(AdTreeElementType.Dependent, adTree.Dependent.Dependent.ElementType);
        }
    }
}
