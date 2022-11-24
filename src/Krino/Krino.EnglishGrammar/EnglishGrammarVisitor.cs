using Antlr4.Runtime.Misc;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Krino.EnglishGrammar
{
    public class EnglishGrammarVisitor : EnglishBaseVisitor<string>
    {
        public override string VisitAdverbPhrase([NotNull] EnglishParser.AdverbPhraseContext context)
        {
            return base.VisitAdverbPhrase(context);
        }
    }
}
