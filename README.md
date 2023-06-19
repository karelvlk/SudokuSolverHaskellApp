# Dokumentace k programu Sudoku Solver Haskell App

## O aplikaci
"Sudoku Solver Haskell App" je komplexní aplikace pro generování, řešení a kontrolu správnosti Sudoku puzzle. Aplikace je napsána v jazyce Haskell a je strukturována do různých modulů, které se starají o různé funkce aplikace. Aplikace poskytuje uživatelům tři režimy - "Generátor", "Řešitel" a "Kontrolor".

## Uživatelská dokumentace
Spustit aplikaci Sudoku Solver Haskell App můžete z příkazové řádky pomocí příkazu runghc main.hs.

Po spuštění aplikace se objeví uvítací zpráva a výzva k výběru módu:

```markdown
Welcome to Sudoku Solver Haskell App!
Please choose the mode:
1. Generator Mode
2. Solver Mode
3. Checker Mode
```

Poté uživatel zadá číslo módu, který chce použít. Každý mód má své vlastní možnosti, které jsou uživateli prezentovány po výběru módu.

Například, v módu řešitele (Solver Mode), může uživatel zadat Sudoku ručně nebo načíst z existujícího souboru. V módu generátoru (Generator Mode) si může uživatel vybrat obtížnost nebo počet buněk, které chce vynechat. A v módu kontroly (Checker Mode) může uživatel zadat Sudoku ručně nebo načíst z existujícího souboru.

## Detailní technická specifikace
Program je složen z několika modulů, které obsahují různé funkce pro manipulaci s daty a implementaci algoritmů.

### Moduly aplikace:
- Modul Main: tento modul obsahuje hlavní funkci aplikace, která je zodpovědná za uvítání uživatele a výběr módu aplikace.
- Modul Solver: tento modul obsahuje funkci pro řešení Sudoku. Uživatel může zadat Sudoku ručně nebo načíst ze souboru. Dále obsahuje funkce pro nalezení prázdné buňky, vyplnění prázdné buňky platným číslem a kontrolu, zda je číslo v buňce platné.
- Modul Generator: tento modul obsahuje funkci pro generování Sudoku. Uživatel může vybrat obtížnost nebo počet buněk, které chce vynechat. Dále obsahuje funkce pro generování plné mřížky a odstranění buněk.
- Modul Checker: tento modul obsahuje funkci pro kontrolu Sudoku. Uživatel může zadat Sudoku ručně nebo načíst ze souboru. Dále obsahuje funkci pro kontrolu, zda je mřížka platná.
- Modul Utils: tento pomocný modul obsahuje funkce pro načítání mřížky ze souboru a tisk mřížky.
- Modul Types: tento modul definuje různé datové typy používané v aplikaci, včetně Grid, Cell, Mode, SolverMode, Difficulty a CheckerMode.

### Hlavní funkce a algoritmy
#### Backtracking algoritmus
Hlavní funkci pro řešení Sudoku nalezneme v modulu solver.hs. Tato funkce se jmenuje solveSudoku a používá backtracking algoritmus.

Algoritmus pracuje následovně:

- Prochází prázdné buňky v Sudoku od levého horního rohu po pravý dolní roh.
- Pro každou prázdnou buňku zkouší všechna čísla od 1 do 9 a kontroluje, zda je možné dané číslo vložit do buňky podle pravidel Sudoku.
- Pokud je možné číslo vložit, algoritmus tak učiní a pokračuje k další prázdné buňce.
- Pokud není možné vložit žádné číslo, algoritmus se vrátí o buňku zpět (backtrack) a pokusí se vložit další číslo.
- Pokud algoritmus projde všechny buňky a všechna čísla, Sudoku je vyřešeno.

#### Generování Sudoku
Generování nové Sudoku hádanky probíhá v modulu generator.hs. Základem je plně vyplněná a platná Sudoku mřížka, ze které jsou náhodně odstraňovány buňky až do požadované úrovně obtížnosti.

#### Validace Sudoku
Modul checker.hs obsahuje funkce pro validaci řešení Sudoku. Funkce `check` přijímá mřížku Sudoku a ověřuje, zda je řešením platného Sudoku. Pro ověření platnosti řešení se kontroluje, zda jsou splněna pravidla Sudoku, tj. žádné číslo se neopakuje ve stejném řádku, sloupci nebo v bloku 3x3.

#### Manipulace s vstupem a výstupem
Modul utils.hs obsahuje funkce pro čtení a zápis mřížky Sudoku ze souboru a také pro tisk Sudoku do konzole. Funkce `loadGrid` načte mřížku ze souboru a převede ji na interní reprezentaci v programu. Funkce saveGrid uloží mřížku do souboru. Funkce `printGrid` tiskne mřížku Sudoku do konzole.

## Závěr
Sudoku Solver Haskell App je efektivní a uživatelsky přívětivá aplikace pro řešení, generování a kontrolu úloh Sudoku. Aplikace je navržena tak, aby byla snadno použitelná a flexibilní, a umožňuje uživatelům vybrat mezi různými módy a možnostmi. Uživatelé si mohou vybrat, jak zadat nebo načíst úlohy Sudoku, a mohou také nastavit různé parametry pro generování úloh. Výsledkem je nástroj, který usnadňuje řešení, generování a kontrolu úloh Sudoku.