using Krino.ConstructiveGrammar.Syntax;

namespace Krino.App.Services
{
    public interface IGrammarService
    {
        Task<Parser> CreateParser();
    }
}
