# Program to calculate how long it takes to pay off debt

# Given starting balance and monthly payment amount,
# returns end balance after given period
def end_bal(balance, annual_interest, payment, num_months):
    while num_months > 0:
        balance = balance + (annual_interest/12)*(balance - payment) - payment
        num_months -= 1
    return balance

# Returns total amount paid to interest during given period
def sum_interest(balance, annual_interest, payment, num_months):
    acc = 0
    while num_months > 0:
        acc += (annual_interest/12)*(balance - payment)
        balance = balance + (annual_interest/12)*(balance - payment) - payment
        num_months -= 1
    return acc

# Returns lowest monthly payment to pay off balance within given period
def lowest_payment(balance, annual_interest, num_months):
    low = balance / num_months
    high = balance * (1 + annual_interest)**12 / num_months
    payment = (low + high) / 2.0
    while abs(end_bal(balance, annual_interest, payment, num_months)) > 0.01:
        if end_bal(balance, annual_interest, payment, num_months) > 0:
            low = payment
        else:
            high = payment
        payment = (low + high)/2.0
    return payment

# Main procedure
owed = float(input("Enter how much you owe: "))
interest = float(input("Enter annual interest rate (decimal): "))
months = int(input("Enter the number of months to pay off debt: "))
lowest = lowest_payment(owed, interest, months)
total = sum_interest(owed, interest, lowest, months)
print("You can pay off your debt in", months, "months by paying",
      round(lowest, 2), "every month.\nYou'll have paid", round(total, 2),
      "in interest.")

