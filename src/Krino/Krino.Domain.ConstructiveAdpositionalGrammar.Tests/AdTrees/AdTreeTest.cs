using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Patterns;
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
                Right = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    Left = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                Left = new AdTree()
                {
                    Morpheme = new Morpheme("") { GrammarCharacter = GrammarCharacter.U },

                    Right = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    Left = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };


            Assert.AreEqual(GrammarCharacter.Epsilon, adTree.GrammarCharacter);
            Assert.AreEqual(GrammarCharacter.O, adTree.Right.Left.GrammarCharacter);
            Assert.AreEqual(GrammarCharacter.Epsilon, adTree.Right.GrammarCharacter);
            Assert.AreEqual(GrammarCharacter.U, adTree.Left.GrammarCharacter);
        }

        [Test]
        public void InheritedGrammarCharacter()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                Right = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    Left = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                Left = new AdTree()
                {
                    Morpheme = new Morpheme("") { GrammarCharacter = GrammarCharacter.U },

                    Right = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    Left = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };


            Assert.AreEqual(GrammarCharacter.I, adTree.InheritedGrammarCharacter);
            Assert.AreEqual(GrammarCharacter.Epsilon, adTree.Right.Left.InheritedGrammarCharacter);
            Assert.AreEqual(GrammarCharacter.I, adTree.Right.InheritedGrammarCharacter);

            Assert.AreEqual(GrammarCharacter.O, adTree.Left.InheritedGrammarCharacter);
        }

        [Test]
        public void AdPosition_IncorrectParent_Exception()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                Right = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    Left = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                Left = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    Left = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            AdTree adTree2 = new AdTree()
            {
                Right = new AdTree(),
                Left = new AdTree(),
            };

            // The adTree2 does not references the adTree.RightChild.RightChild therefore it shall throw the exception.
            Assert.Throws<InvalidOperationException>(() => adTree.Right.Right.AdPosition = adTree2);
        }

        [Test]
        public void IsAdPosition()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                Right = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    Left = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                Left = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    Left = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            Assert.IsTrue(adTree.IsAdPosition);
            Assert.IsTrue(adTree.Right.IsAdPosition);
            Assert.IsTrue(adTree.Left.IsAdPosition);

            Assert.IsFalse(adTree.Left.Right.IsAdPosition);
            Assert.IsFalse(adTree.Left.Left.IsAdPosition);
        }

        [Test]
        public void AdPositions()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                Right = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    Left = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                Left = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    Left = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            List<IAdTree> adPositions = adTree.AdPositions.ToList();
            Assert.AreEqual(0, adPositions.Count);

            adPositions = adTree.Right.Right.AdPositions.ToList();
            Assert.AreEqual(2, adPositions.Count);
            Assert.AreEqual(adTree.Right, adPositions[0]);
            Assert.AreEqual(adTree, adPositions[1]);
        }


        [Test]
        public void RightChild()
        {
            AdTree adTree = new AdTree();
            Assert.IsNull(adTree.Right);

            AdTree rightChild = new AdTree();
            
            // Attach the governor.
            adTree.Right = rightChild;
            Assert.AreEqual(rightChild, adTree.Right);
            Assert.AreEqual(adTree, rightChild.AdPosition);

            // Detach the governor.
            adTree.Right = null;
            Assert.IsNull(adTree.Right);
            Assert.IsNull(rightChild.AdPosition);
        }

        [Test]
        public void LeftChild()
        {
            AdTree adTree = new AdTree();
            Assert.IsNull(adTree.Left);

            AdTree leftChild = new AdTree();

            // Attach dependent.
            adTree.Left = leftChild;
            Assert.AreEqual(leftChild, adTree.Left);
            Assert.AreEqual(adTree, leftChild.AdPosition);

            // Detach the dependent.
            adTree.Left = null;
            Assert.IsNull(adTree.Left);
            Assert.IsNull(leftChild.AdPosition);
        }


        [Test]
        public void RightChildren()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                Right = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    Left = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                Left = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    Left = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            List<IAdTree> rightChildren = adTree.RightChildren.ToList();
            Assert.AreEqual(2, rightChildren.Count);
            Assert.AreEqual(adTree.Right, rightChildren[0]);
            Assert.AreEqual(adTree.Right.Right, rightChildren[1]);


            rightChildren = adTree.Right.Right.RightChildren.ToList();
            Assert.AreEqual(0, rightChildren.Count);
        }

        [Test]
        public void IsOnRight()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                Right = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    Left = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                Left = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    Left = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            Assert.IsTrue(adTree.Right.IsOnRight);
            Assert.IsFalse(adTree.Left.IsOnRight);
            Assert.IsFalse(adTree.IsOnRight);
        }

        [Test]
        public void IsOnLeft()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                Right = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    Left = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                Left = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    Left = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            Assert.IsFalse(adTree.Right.IsOnLeft);
            Assert.IsTrue(adTree.Left.IsOnLeft);
            Assert.IsFalse(adTree.IsOnLeft);
        }

        [Test]
        public void Governor()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                Right = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    Left = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                Left = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    Left = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            // the governor of 'I' is the 'read'.
            Assert.IsTrue(adTree.Right.Right == adTree.Right.Left.Governor);

            // the governor of 'book'-adposition is the 'read'.
            Assert.IsTrue(adTree.Right.Right == adTree.Left.Governor);

            // the governor of 'book' is the 'read'.
            // Note: the book is already the governor so the governor of the governor shall be found.
            Assert.IsTrue(adTree.Right.Right == adTree.Left.Right.Governor);

            // the governor of 'the' is the 'book'.
            Assert.IsTrue(adTree.Left.Right == adTree.Left.Left.Governor);

            // the governor of the root is the 'read'.
            Assert.IsTrue(adTree.Right.Right == adTree.Governor);

            // the governor of the 'read'-adposition is the 'read'.
            Assert.IsTrue(adTree.Right.Right == adTree.Right.Governor);
        }

        [Test]
        public void Governor2()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                Right = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("hello") { GrammarCharacter = GrammarCharacter.Epsilon } },
                },
                Left = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    Left = new AdTree()
                    {
                        Right = new AdTree() { Morpheme = new Morpheme("and") { GrammarCharacter = GrammarCharacter.U } },
                        Left = new AdTree() { Morpheme = new Morpheme("bla") { GrammarCharacter = GrammarCharacter.E } }
                    }
                }
            };

            // the governor of 'bla' is the 'book'. (adposition U - 'bla' shall be ignored.
            Assert.IsTrue(adTree.Left.Right == adTree.Left.Left.Left.Governor);

            // the 'book' does not have any governor.
            Assert.IsTrue(adTree.Left.Right.Governor == null);

            // the root does not have any governor.
            Assert.IsTrue(adTree.Governor == null);
        }

        [Test]
        public void IsGovernor()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                Right = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    Left = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                Left = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    Left = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            // 'read' is governor.
            Assert.IsTrue(adTree.Right.Right.IsGovernor);

            // 'book'-adposition is not the governor.
            Assert.IsFalse(adTree.Left.IsGovernor);

            // root is not the governor.
            Assert.IsFalse(adTree.IsGovernor);
        }

        [Test]
        public void ISGovernor2()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                Right = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("hello") { GrammarCharacter = GrammarCharacter.Epsilon } },
                },
                Left = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    Left = new AdTree()
                    {
                        Right = new AdTree() { Morpheme = new Morpheme("and") { GrammarCharacter = GrammarCharacter.U } },
                        Left = new AdTree() { Morpheme = new Morpheme("bla") { GrammarCharacter = GrammarCharacter.E } }
                    }
                }
            };

            // hello is not the governor.
            Assert.IsFalse(adTree.Right.Right.IsGovernor);

            // adposition 'and' is not the governor.
            Assert.IsFalse(adTree.Left.Left.Right.IsGovernor);
        }

        [Test]
        public void DependentAdPositions()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                Right = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    Left = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                Left = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    Left = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            // dependents for 'read'
            List<IAdTree> dependentAdPositions = adTree.Right.Right.DependentAdPositions.ToList();
            Assert.AreEqual(2, dependentAdPositions.Count);
            Assert.IsTrue(adTree.Right == dependentAdPositions[0]);
            Assert.IsTrue(adTree == dependentAdPositions[1]);

            // dependents for 'book'
            dependentAdPositions = adTree.Left.Right.DependentAdPositions.ToList();
            Assert.AreEqual(1, dependentAdPositions.Count);
            Assert.IsTrue(adTree.Left == dependentAdPositions[0]);
        }

        [Test]
        public void ValencyAdPosition()
        {
            AdTree adTree = new AdTree()
            {
                Pattern = new Pattern() { ValencyPosition = 2 },

                Right = new AdTree()
                {
                    Pattern = new Pattern() { ValencyPosition = 1 },

                    Right = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    Left = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                Left = new AdTree()
                {
                    Right = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    Left = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            //'I'
            Assert.IsTrue(adTree.Right == adTree.Right.Left.ValencyAdPosition);

            // book
            Assert.IsTrue(adTree == adTree.Left.Right.ValencyAdPosition);

            // the
            Assert.IsTrue(adTree == adTree.Left.Left.ValencyAdPosition);

            // book-adposition shall return root
            Assert.IsTrue(adTree == adTree.Left.ValencyAdPosition);

            // root has defined the valency position so it should return itself
            Assert.IsTrue(adTree == adTree.ValencyAdPosition);

            // read shall return null - because it is the verb
            Assert.IsTrue(null == adTree.Right.Right.ValencyAdPosition);

            Assert.That(true, Is.True);
        }



        [Test]
        public void Phrase()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                Morpheme = new Morpheme("."),

                Right = new AdTree()
                {
                    Right = new AdTree()
                    {
                        Pattern = new Pattern() { IsReversed = true },

                        Right = new AdTree() { Morpheme = new Morpheme("read") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                        Left = new AdTree() { Morpheme = new Morpheme("I") { GrammarCharacter = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                    },
                    Left = new AdTree()
                    {
                        Pattern = new Pattern() { IsReversed = true },

                        Right = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                        Left = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                    }
                }
            };

            string phrase = adTree.Phrase;
            Assert.AreEqual("I read the book .", phrase);
        }

        [Test]
        public void Phrase_Sentence_8_3()
        {
            // The phrase:  That amount of carbon dioxide will run out in about 12 years of current business as usual.
            // Note: Sentence 8.3 page 32.
            AdTree adTree = new AdTree()
            {
                Morpheme = new Morpheme(".") { GrammarCharacter = GrammarCharacter.U },

                Right = new AdTree()
                {
                    Morpheme = new Morpheme("in") { GrammarCharacter = GrammarCharacter.U },

                    Right = new AdTree()
                    {
                        Pattern = new Pattern() { ValencyPosition = 1, IsReversed = true },

                        Right = new AdTree()
                        {
                            Right = new AdTree()
                            {
                                Pattern = new Pattern() { IsReversed = true },

                                Right = new AdTree() { Morpheme = new Morpheme("run") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Monovalent } },
                                Left = new AdTree() { Morpheme = new Morpheme("will") { GrammarCharacter = GrammarCharacter.E } }
                            },

                            Left = new AdTree() { Morpheme = new Morpheme("out") { GrammarCharacter = GrammarCharacter.E } },
                        },

                        Left = new AdTree()
                        {
                            Morpheme = new Morpheme("of") { GrammarCharacter = GrammarCharacter.U },

                            Right = new AdTree()
                            {
                                Pattern = new Pattern() { IsReversed = true },

                                Right = new AdTree() { Morpheme = new Morpheme("amount") { GrammarCharacter = GrammarCharacter.O } },
                                Left = new AdTree() { Morpheme = new Morpheme("That") { GrammarCharacter = GrammarCharacter.A } },
                            },

                            Left = new AdTree()
                            {
                                Right = new AdTree() { Morpheme = new Morpheme("carbon") { GrammarCharacter = GrammarCharacter.O } },
                                Left = new AdTree() { Morpheme = new Morpheme("dioxide") { GrammarCharacter = GrammarCharacter.A } },
                            }
                        }
                    },

                    Left = new AdTree()
                    {
                        Morpheme = new Morpheme("of") { GrammarCharacter = GrammarCharacter.U },

                        Right = new AdTree()
                        {
                            Pattern = new Pattern() { IsReversed = true },

                            Right = new AdTree()
                            {
                                Pattern = new Pattern() { IsReversed = true },

                                Right = new AdTree() { Morpheme = new Morpheme("years") { GrammarCharacter = GrammarCharacter.O } },
                                Left = new AdTree() { Morpheme = new Morpheme("12") { GrammarCharacter = GrammarCharacter.A } },
                            },

                            Left = new AdTree() { Morpheme = new Morpheme("about") { GrammarCharacter = GrammarCharacter.A } },
                        },

                        Left = new AdTree()
                        {
                            Pattern = new Pattern() { IsReversed = true },

                            Right = new AdTree()
                            {
                                Right = new AdTree()
                                {
                                    Right = new AdTree() { Morpheme = new Morpheme("business") { GrammarCharacter = GrammarCharacter.O } },
                                    Left = new AdTree() { Morpheme = new Morpheme("as") { GrammarCharacter = GrammarCharacter.A } },
                                },

                                Left = new AdTree() { Morpheme = new Morpheme("usual") { GrammarCharacter = GrammarCharacter.A } },
                            },

                            Left = new AdTree() { Morpheme = new Morpheme("current") { GrammarCharacter = GrammarCharacter.A } },
                        }
                    }
                }
            };

            string phrase = adTree.Phrase;
            Assert.AreEqual("That amount of carbon dioxide will run out in about 12 years of current business as usual .", phrase);
        }


        [Test]
        public void Phrase_Correlative()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                Pattern = new Pattern() { IsCorrelativeAdposition = true },
                Morpheme = new Morpheme("which") { GrammarCharacter = GrammarCharacter.U },

                Right = new AdTree()
                {
                    Pattern = new Pattern { IsReversed = true },
                    Right = new AdTree() { Morpheme = new Morpheme("book") { GrammarCharacter = GrammarCharacter.O } },
                    Left = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A } },
                },

                Left = new AdTree()
                {
                    Morpheme = new Morpheme("for") { GrammarCharacter = GrammarCharacter.U },

                    Right = new AdTree()
                    {
                        Pattern = new Pattern() { ValencyPosition = 2 },
                        Right = new AdTree()
                        {
                            Pattern = new Pattern() { ValencyPosition = 1, IsReversed = true },
                            Right = new AdTree() { Morpheme = new Morpheme("bought") { GrammarCharacter = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                            Left = new AdTree()
                            {
                                Pattern = new Pattern { IsReversed = true },
                                Right = new AdTree() { Morpheme = new Morpheme("man") { GrammarCharacter = GrammarCharacter.O } },
                                Left = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A } },
                            }
                        },
                        Left = new AdTree()
                        {
                            Pattern = new Pattern() { IsCorrelativeSubstitute = true },
                            Morpheme = new Morpheme("") { GrammarCharacter = GrammarCharacter.O }
                        }
                    },
                    Left = new AdTree()
                    {
                        Pattern = new Pattern() { IsReversed = true },
                        Right = new AdTree() { Morpheme = new Morpheme("girl") { GrammarCharacter = GrammarCharacter.O } },
                        Left = new AdTree() { Morpheme = new Morpheme("the") { GrammarCharacter = GrammarCharacter.A } },
                    }
                }

            };

            string phrase = adTree.Phrase;
            Assert.AreEqual("the book which the man bought for the girl", phrase);
        }
    }
}
