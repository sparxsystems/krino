using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.AdTrees
{
    [TestFixture]
    public class AdTreeTest
    {
        [Test]
        public void GrammarCharacterProperty()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                RightChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                LeftChild = new AdTree()
                {
                    Morpheme = new Morpheme("") { GrammarCharacter = GrammarCharacter.U },

                    RightChild = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };


            Assert.AreEqual(GrammarCharacter.Epsilon, adTree.GrammarCharacter);
            Assert.AreEqual(GrammarCharacter.O, adTree.RightChild.LeftChild.GrammarCharacter);
            Assert.AreEqual(GrammarCharacter.Epsilon, adTree.RightChild.GrammarCharacter);
            Assert.AreEqual(GrammarCharacter.U, adTree.LeftChild.GrammarCharacter);
        }

        [Test]
        public void InheritedGrammarCharacter()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                RightChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                LeftChild = new AdTree()
                {
                    Morpheme = new Morpheme("") { GrammarCharacter = GrammarCharacter.U },

                    RightChild = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };


            Assert.AreEqual(GrammarCharacter.I, adTree.InheritedGrammarCharacter);
            Assert.AreEqual(GrammarCharacter.Epsilon, adTree.RightChild.LeftChild.InheritedGrammarCharacter);
            Assert.AreEqual(GrammarCharacter.I, adTree.RightChild.InheritedGrammarCharacter);

            Assert.AreEqual(GrammarCharacter.O, adTree.LeftChild.InheritedGrammarCharacter);
        }

        [Test]
        public void AdPosition_IncorrectParent_Exception()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                RightChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                LeftChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            AdTree adTree2 = new AdTree()
            {
                RightChild = new AdTree(),
                LeftChild = new AdTree(),
            };

            // The adTree2 does not references the adTree.RightChild.RightChild therefore it shall throw the exception.
            Assert.Throws<InvalidOperationException>(() => adTree.RightChild.RightChild.AdPosition = adTree2);
        }

        [Test]
        public void IsAdPosition()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                RightChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                LeftChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            Assert.IsTrue(adTree.IsAdPosition);
            Assert.IsTrue(adTree.RightChild.IsAdPosition);
            Assert.IsTrue(adTree.LeftChild.IsAdPosition);

            Assert.IsFalse(adTree.LeftChild.RightChild.IsAdPosition);
            Assert.IsFalse(adTree.LeftChild.LeftChild.IsAdPosition);
        }

        [Test]
        public void AdPositions()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                RightChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                LeftChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            List<IAdTree> adPositions = adTree.AdPositions.ToList();
            Assert.AreEqual(0, adPositions.Count);

            adPositions = adTree.RightChild.RightChild.AdPositions.ToList();
            Assert.AreEqual(2, adPositions.Count);
            Assert.AreEqual(adTree.RightChild, adPositions[0]);
            Assert.AreEqual(adTree, adPositions[1]);
        }


        [Test]
        public void RightChild()
        {
            AdTree adTree = new AdTree();
            Assert.IsNull(adTree.RightChild);

            AdTree rightChild = new AdTree();
            
            // Attach the governor.
            adTree.RightChild = rightChild;
            Assert.AreEqual(rightChild, adTree.RightChild);
            Assert.AreEqual(adTree, rightChild.AdPosition);

            // Detach the governor.
            adTree.RightChild = null;
            Assert.IsNull(adTree.RightChild);
            Assert.IsNull(rightChild.AdPosition);
        }

        [Test]
        public void LeftChild()
        {
            AdTree adTree = new AdTree();
            Assert.IsNull(adTree.LeftChild);

            AdTree leftChild = new AdTree();

            // Attach dependent.
            adTree.LeftChild = leftChild;
            Assert.AreEqual(leftChild, adTree.LeftChild);
            Assert.AreEqual(adTree, leftChild.AdPosition);

            // Detach the dependent.
            adTree.LeftChild = null;
            Assert.IsNull(adTree.LeftChild);
            Assert.IsNull(leftChild.AdPosition);
        }


        [Test]
        public void RightChildren()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                RightChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                LeftChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            List<IAdTree> rightChildren = adTree.RightChildren.ToList();
            Assert.AreEqual(2, rightChildren.Count);
            Assert.AreEqual(adTree.RightChild, rightChildren[0]);
            Assert.AreEqual(adTree.RightChild.RightChild, rightChildren[1]);


            rightChildren = adTree.RightChild.RightChild.RightChildren.ToList();
            Assert.AreEqual(0, rightChildren.Count);
        }

        [Test]
        public void IsOnRight()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                RightChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                LeftChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            Assert.IsTrue(adTree.RightChild.IsOnRight);
            Assert.IsFalse(adTree.LeftChild.IsOnRight);
            Assert.IsFalse(adTree.IsOnRight);
        }

        [Test]
        public void IsOnLeft()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                RightChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                LeftChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            Assert.IsFalse(adTree.RightChild.IsOnLeft);
            Assert.IsTrue(adTree.LeftChild.IsOnLeft);
            Assert.IsFalse(adTree.IsOnLeft);
        }

        [Test]
        public void Governor()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                RightChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                LeftChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            // the governor of 'I' is the 'read'.
            Assert.IsTrue(adTree.RightChild.RightChild == adTree.RightChild.LeftChild.Governor);

            // the governor of 'book'-adposition is the 'read'.
            Assert.IsTrue(adTree.RightChild.RightChild == adTree.LeftChild.Governor);

            // the governor of 'book' is the 'read'.
            // Note: the book is already the governor so the governor of the governor shall be found.
            Assert.IsTrue(adTree.RightChild.RightChild == adTree.LeftChild.RightChild.Governor);

            // the governor of 'the' is the 'book'.
            Assert.IsTrue(adTree.LeftChild.RightChild == adTree.LeftChild.LeftChild.Governor);

            // the governor of the root is the 'read'.
            Assert.IsTrue(adTree.RightChild.RightChild == adTree.Governor);

            // the governor of the 'read'-adposition is the 'read'.
            Assert.IsTrue(adTree.RightChild.RightChild == adTree.RightChild.Governor);
        }

        [Test]
        public void Governor2()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                RightChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("hello") { GrammarCharacter = GrammarCharacter.Epsilon } },
                },
                LeftChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    LeftChild = new AdTree()
                    {
                        RightChild = new AdTree() { Morpheme = new Morpheme("and") { GrammarCharacter = GrammarCharacter.U } },
                        LeftChild = new AdTree() { Morpheme = new Morpheme("bla") { GrammarCharacter = GrammarCharacter.E } }
                    }
                }
            };

            // the governor of 'bla' is the 'book'. (adposition U - 'bla' shall be ignored.
            Assert.IsTrue(adTree.LeftChild.RightChild == adTree.LeftChild.LeftChild.LeftChild.Governor);

            // the 'book' does not have any governor.
            Assert.IsTrue(adTree.LeftChild.RightChild.Governor == null);

            // the root does not have any governor.
            Assert.IsTrue(adTree.Governor == null);
        }

        [Test]
        public void IsGovernor()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                RightChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                LeftChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            // 'read' is governor.
            Assert.IsTrue(adTree.RightChild.RightChild.IsGovernor);

            // 'book'-adposition is not the governor.
            Assert.IsFalse(adTree.LeftChild.IsGovernor);

            // root is not the governor.
            Assert.IsFalse(adTree.IsGovernor);
        }

        [Test]
        public void ISGovernor2()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                RightChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("hello") { GrammarCharacter = GrammarCharacter.Epsilon } },
                },
                LeftChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    LeftChild = new AdTree()
                    {
                        RightChild = new AdTree() { Morpheme = new Morpheme("and") { GrammarCharacter = GrammarCharacter.U } },
                        LeftChild = new AdTree() { Morpheme = new Morpheme("bla") { GrammarCharacter = GrammarCharacter.E } }
                    }
                }
            };

            // hello is not the governor.
            Assert.IsFalse(adTree.RightChild.RightChild.IsGovernor);

            // adposition 'and' is not the governor.
            Assert.IsFalse(adTree.LeftChild.LeftChild.RightChild.IsGovernor);
        }

        [Test]
        public void DependentAdPositions()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                RightChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                LeftChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            // dependents for 'read'
            List<IAdTree> dependentAdPositions = adTree.RightChild.RightChild.DependentAdPositions.ToList();
            Assert.AreEqual(2, dependentAdPositions.Count);
            Assert.IsTrue(adTree.RightChild == dependentAdPositions[0]);
            Assert.IsTrue(adTree == dependentAdPositions[1]);

            // dependents for 'book'
            dependentAdPositions = adTree.LeftChild.RightChild.DependentAdPositions.ToList();
            Assert.AreEqual(1, dependentAdPositions.Count);
            Assert.IsTrue(adTree.LeftChild == dependentAdPositions[0]);
        }

        [Test]
        public void ValencyAdPosition()
        {
            AdTree adTree = new AdTree()
            {
                ValencyPosition = 2,

                RightChild = new AdTree()
                {
                    ValencyPosition = 1,

                    RightChild = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                LeftChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            //'I'
            Assert.IsTrue(adTree.RightChild == adTree.RightChild.LeftChild.ValencyAdPosition);

            // book
            Assert.IsTrue(adTree == adTree.LeftChild.RightChild.ValencyAdPosition);

            // the
            Assert.IsTrue(adTree == adTree.LeftChild.LeftChild.ValencyAdPosition);

            // book-adposition shall return root
            Assert.IsTrue(adTree == adTree.LeftChild.ValencyAdPosition);

            // root has defined the valency position so it should return itself
            Assert.IsTrue(adTree == adTree.ValencyAdPosition);

            // read shall return null - because it is the verb
            Assert.IsTrue(null == adTree.RightChild.RightChild.ValencyAdPosition);
        }



        [Test]
        public void Phrase()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                RightChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                LeftChild = new AdTree()
                {
                    RightChild = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    LeftChild = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            string phrase = string.Join(" ", adTree.Phrase.Select(x => x.Morpheme.Morph));
        }
    }
}
