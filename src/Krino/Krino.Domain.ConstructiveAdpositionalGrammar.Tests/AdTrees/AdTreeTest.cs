using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural;
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
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read") { Attributes = Attributes.I.Lexeme.Verb }, new Pattern()),
                    Left = new AdTree(new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun }, new Pattern())
                },
                Left = new AdTree(new Morpheme("") { Attributes = Attributes.U }, new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                    Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner }, new Pattern())
                }
            };


            Assert.AreEqual(GrammarCharacter.Epsilon, adTree.Morpheme.GrammarCharacter);
            Assert.AreEqual(GrammarCharacter.O, adTree.Right.Left.Morpheme.GrammarCharacter);
            Assert.AreEqual(GrammarCharacter.Epsilon, adTree.Right.Morpheme.GrammarCharacter);
            Assert.AreEqual(GrammarCharacter.U, adTree.Left.Morpheme.GrammarCharacter);
        }

        [Test]
        public void InheritedGrammarCharacter()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read") { Attributes  = Attributes.I.Lexeme.Verb }, new Pattern()),
                    Left = new AdTree(new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun }, new Pattern())
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                    Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner }, new Pattern())
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
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read") { Attributes  = Attributes.I.Lexeme.Verb }, new Pattern()),
                    Left = new AdTree(new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun }, new Pattern())
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                    Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner }, new Pattern())
                }
            };

            AdTree adTree2 = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern()),
                Left = new AdTree(new Morpheme(""), new Pattern()),
            };

            // The adTree2 does not references the adTree.RightChild.RightChild therefore it shall throw the exception.
            Assert.Throws<InvalidOperationException>(() => adTree.Right.Right.AdPosition = adTree2);

            // Also it shall not be possible to assign null.
            Assert.Throws<InvalidOperationException>(() => adTree.Right.Right.AdPosition = null);
        }

        [Test]
        public void IsAdPosition()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read") { Attributes  = Attributes.I.Lexeme.Verb }, new Pattern()),
                    Left = new AdTree(new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun }, new Pattern())
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                    Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner }, new Pattern())
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
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read") { Attributes  = Attributes.I.Lexeme.Verb }, new Pattern()),
                    Left = new AdTree(new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun }, new Pattern())
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                    Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner }, new Pattern())
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
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern());
            Assert.IsNull(adTree.Right);

            AdTree rightChild = new AdTree(new Morpheme(""), new Pattern());
            
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
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern());
            Assert.IsNull(adTree.Left);

            AdTree leftChild = new AdTree(new Morpheme(""), new Pattern());

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
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read") { Attributes  = Attributes.I.Lexeme.Verb }, new Pattern()),
                    Left = new AdTree(new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun }, new Pattern())
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                    Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner }, new Pattern())
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
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read") { Attributes  = Attributes.I.Lexeme.Verb }, new Pattern()),
                    Left = new AdTree(new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun }, new Pattern())
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                    Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner }, new Pattern())
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
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read") { Attributes  = Attributes.I.Lexeme.Verb }, new Pattern()),
                    Left = new AdTree(new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun }, new Pattern())
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                    Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner }, new Pattern())
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
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read") { Attributes  = Attributes.I.Lexeme.Verb }, new Pattern()),
                    Left = new AdTree(new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun }, new Pattern())
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                    Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner }, new Pattern())
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
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    // note: it is empty to ensure the root does not have any governor.
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                    Left = new AdTree(new Morpheme(""), new Pattern())
                    {
                        Right = new AdTree(new Morpheme("and") { Attributes = Attributes.U.Lexeme.Conjunction }, new Pattern()),
                        Left = new AdTree(new Morpheme("bla") { Attributes = Attributes.E.Lexeme.Adverb }, new Pattern())
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
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read") { Attributes  = Attributes.I.Lexeme.Verb }, new Pattern()),
                    Left = new AdTree(new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun }, new Pattern())
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                    Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner }, new Pattern())
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
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("hello"), new Pattern()),
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                    Left = new AdTree(new Morpheme(""), new Pattern())
                    {
                        Right = new AdTree(new Morpheme("and") { Attributes = Attributes.U.Lexeme.Conjunction }, new Pattern()),
                        Left = new AdTree(new Morpheme("bla") { Attributes = Attributes.E.Lexeme.Adverb }, new Pattern())
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
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read") { Attributes  = Attributes.I.Lexeme.Verb }, new Pattern()),
                    Left = new AdTree(new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun }, new Pattern())
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                    Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner }, new Pattern())
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
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern() { PatternAttributes = PatternAttributes.ValencyPosition.Second })
            {
                Right = new AdTree(new Morpheme(""), new Pattern() { PatternAttributes = PatternAttributes.ValencyPosition.First })
                {
                    Right = new AdTree(new Morpheme("read") { Attributes = Attributes.I.Lexeme.Verb.Bivalent }, new Pattern() { MorphemeRule = MorphemeRule.I }),
                    Left = new AdTree(new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun }, new Pattern() { MorphemeRule = MorphemeRule.O })
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern() { MorphemeRule = MorphemeRule.O }),
                    Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner }, new Pattern() { MorphemeRule = MorphemeRule.A })
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
            AdTree adTree = new AdTree(new Morpheme("."), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern() { PatternAttributes = PatternAttributes.ValencyPosition.Second })
                {
                    Right = new AdTree(new Morpheme(""), new Pattern() { PatternAttributes = PatternAttributes.ValencyPosition.First })
                    {
                        Right = new AdTree(new Morpheme("read") { Attributes = Attributes.I.Lexeme.Verb.Bivalent | Attributes.I.Lexeme.Verb.Unergative }, new Pattern()),
                        Left = new AdTree(new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun.Subjective }, new Pattern())
                    },
                    Left = new AdTree(new Morpheme(""), new Pattern())
                    {
                        Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun.Common.Concrete }, new Pattern()),
                        Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner.DefiniteArticle }, new Pattern())
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
            AdTree adTree = new AdTree(new Morpheme("."), new Pattern())
            {
                Right = new AdTree(new Morpheme("in") { Attributes = Attributes.E.Lexeme.Preposition }, new Pattern())
                {
                    Right = new AdTree(new Morpheme(""), new Pattern() { PatternAttributes = PatternAttributes.ValencyPosition.First, })
                    {
                        Right = new AdTree(new Morpheme(""), new Pattern())
                        {
                            Right = new AdTree(new Morpheme(""), new Pattern())
                            {
                                Right = new AdTree(new Morpheme("run") { Attributes = Attributes.I.Lexeme.Verb.Monovalent }, new Pattern()),
                                Left = new AdTree(new Morpheme("will") { Attributes = Attributes.I.Lexeme.Verb.Modal }, new Pattern())
                            },

                            Left = new AdTree(new Morpheme("out") { Attributes = Attributes.E.Lexeme.Adverb }, new Pattern()),
                        },

                        Left = new AdTree(new Morpheme("of") { Attributes = Attributes.E.Lexeme.Preposition }, new Pattern())
                        {
                            Right = new AdTree(new Morpheme(""), new Pattern())
                            {
                                Right = new AdTree(new Morpheme("amount") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                                Left = new AdTree(new Morpheme("That") { Attributes = Attributes.A.Lexeme.Determiner }, new Pattern()),
                            },

                            Left = new AdTree(new Morpheme(""), new Pattern())
                            {
                                Right = new AdTree(new Morpheme("carbon") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                                Left = new AdTree(new Morpheme("dioxide") { Attributes = Attributes.A.Lexeme.Adjective }, new Pattern()),
                            }
                        }
                    },

                    Left = new AdTree(new Morpheme("of") { Attributes = Attributes.E.Lexeme.Preposition }, new Pattern())
                    {
                        Right = new AdTree(new Morpheme(""), new Pattern())
                        {
                            Right = new AdTree(new Morpheme(""), new Pattern())
                            {
                                Right = new AdTree(new Morpheme("years") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                                Left = new AdTree(new Morpheme("12") { Attributes = Attributes.A.Lexeme.Numeral }, new Pattern()),
                            },

                            Left = new AdTree(new Morpheme("about") { Attributes = Attributes.E.Lexeme.Preposition }, new Pattern()),
                        },

                        Left = new AdTree(new Morpheme(""), new Pattern())
                        {
                            Right = new AdTree(new Morpheme(""), new Pattern())
                            {
                                Right = new AdTree(new Morpheme(""), new Pattern())
                                {
                                    Right = new AdTree(new Morpheme("business") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                                    Left = new AdTree(new Morpheme("as") { Attributes = Attributes.A.Lexeme.Adjective }, new Pattern()),
                                },

                                Left = new AdTree(new Morpheme("usual") { Attributes = Attributes.A.Lexeme.Adjective }, new Pattern()),
                            },

                            Left = new AdTree(new Morpheme("current") { Attributes = Attributes.A.Lexeme.Adjective.Attributive }, new Pattern()),
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
            // The phrase: the book which the man bought for the girl
            AdTree adTree = new AdTree(
                new Morpheme("which") { Attributes = Attributes.O.Lexeme.Pronoun },
                new Pattern() { PatternAttributes = PatternAttributes.CorrelativeAdposition })
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                    Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner.DefiniteArticle }, new Pattern())
                },

                Left = new AdTree(new Morpheme("for") { Attributes = Attributes.E.Lexeme.Preposition }, new Pattern())
                {
                    Right = new AdTree(new Morpheme(""), new Pattern() { PatternAttributes = PatternAttributes.ValencyPosition.Second })
                    {
                        Right = new AdTree(new Morpheme(""), new Pattern() { PatternAttributes = PatternAttributes.ValencyPosition.First })
                        {
                            Right = new AdTree(new Morpheme("bought") { Attributes = Attributes.I.Lexeme.Verb.Bivalent }, new Pattern()),
                            Left = new AdTree(new Morpheme(""), new Pattern())
                            {
                                Right = new AdTree(new Morpheme("man") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                                Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner.DefiniteArticle }, new Pattern()),
                            }
                        },
                        Left = new AdTree(
                            new Morpheme("") { Attributes = Attributes.O.Lexeme.Noun },
                            new Pattern() { PatternAttributes = PatternAttributes.CorrelativeSubstitute })
                        {
                        }
                    },
                    Left = new AdTree(new Morpheme(""), new Pattern())
                    {
                        Right = new AdTree(new Morpheme("girl") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                        Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner.DefiniteArticle }, new Pattern()),
                    }
                }

            };

            string phrase = adTree.Phrase;
            Assert.AreEqual("the book which the man bought for the girl", phrase);
        }


        [Test]
        public void MakeShallowCopy()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read") { Attributes = Attributes.I.Lexeme.Verb }, new Pattern()),
                    Left = new AdTree(new Morpheme("I") { Attributes = Attributes.O.Lexeme.Pronoun }, new Pattern())
                },
                Left = new AdTree(new Morpheme("") { Attributes = Attributes.U }, new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O.Lexeme.Noun }, new Pattern()),
                    Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A.Lexeme.Determiner }, new Pattern())
                }
            };

            IAdTree copy = adTree.MakeShallowCopy();


            Assert.IsTrue(adTree.Morpheme ==  copy.Morpheme);
            Assert.IsTrue(adTree.Pattern == copy.Pattern);

            Assert.IsTrue(adTree.Right.Morpheme == copy.Right.Morpheme);
            Assert.IsTrue(adTree.Right.Pattern == copy.Right.Pattern);

            Assert.IsTrue(adTree.Right.Right.Morpheme == copy.Right.Right.Morpheme);
            Assert.IsTrue(adTree.Right.Right.Pattern == copy.Right.Right.Pattern);

            Assert.IsTrue(adTree.Right.Left.Morpheme == copy.Right.Left.Morpheme);
            Assert.IsTrue(adTree.Right.Left.Pattern == copy.Right.Left.Pattern);

            Assert.IsTrue(adTree.Left.Morpheme == copy.Left.Morpheme);
            Assert.IsTrue(adTree.Left.Pattern == copy.Left.Pattern);

            Assert.IsTrue(adTree.Left.Right.Morpheme == copy.Left.Right.Morpheme);
            Assert.IsTrue(adTree.Left.Right.Pattern == copy.Left.Right.Pattern);

            Assert.IsTrue(adTree.Left.Left.Morpheme == copy.Left.Left.Morpheme);
            Assert.IsTrue(adTree.Left.Left.Pattern == copy.Left.Left.Pattern);
        }
    }
}
